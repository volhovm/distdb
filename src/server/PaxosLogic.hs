-- | Logic for paxos expressed in the ServerM monad terms
module PaxosLogic
       ( replicaOnRequest
       , replicaOnDecision
       , acceptorOnPC
       , spawnScout
       , spawnCommander
       , leaderOnCommit
       , initLeader
       , leaderOnNotification
       ) where

import           Control.Lens             (use, uses, (%=), (+=), (.=), (<%=))
import           Control.Monad            (forM_, unless, when)
import           Control.Monad.RWS.Strict (ask)
import qualified Data.Map                 as M
import           Data.Maybe               (fromJust)
import qualified Data.Set                 as S

import           Communication            (Message (..))
import           PaxosTypes               (Ballot, ClientRequest, Command (..),
                                           Decision (..), LeaderNotification (..), PValue,
                                           PhaseCommitA (..), PhaseCommitB (..), Slot,
                                           aBallotNum, accepted, decisions, lActive,
                                           lBallotNum, lCommanders, lProposals, lScouts,
                                           lUniqueId, proposals, requests, slotIn,
                                           slotOut)
import           ServerTypes              (ServerM, acceptor, hashmap, leader, replica,
                                           serverNodesN, serverPeers, serverPid,
                                           writeMsg')
import           Types                    (Entry (..), EntryRequest (..),
                                           EntryResponse (..))

_WINDOW :: Slot
_WINDOW = 5

applyCommand :: EntryRequest -> ServerM EntryResponse
applyCommand (GetEntry k) =
    uses hashmap (\m -> maybe EntryNotFound (EntryFound . Entry k) (M.lookup k m))
applyCommand (SetEntry (Entry k v)) = do
    hashmap %= M.insert k v
    return EntrySet
applyCommand (DeleteEntry k) = do
    ret <- uses hashmap $ M.member k
    hashmap %= M.delete k
    return $ if ret then EntryDeleted else EntryNotFound


--------- REPLICA ---------

replicaPropose :: ServerM ()
replicaPropose = do
    sIn <- use $ replica . slotIn
    sOut <- use $ replica . slotOut
    reqs <- uses (replica . requests) S.toList
    decs <- use $ replica .decisions
    when (sIn < sOut + _WINDOW && not (null reqs)) $ do
        let c = head reqs
        unless (any (\(s,_) -> s == sIn) decs) $ do
            replica . requests %= S.delete c
            replica . proposals %= S.insert (sIn, c)
            nodes <- serverPeers <$> ask
            forM_ nodes $ \leader -> writeMsg' leader $ ProposeRequest sIn c
        replica . slotIn += 1

replicaPerform :: ClientRequest -> ServerM ()
replicaPerform req@(Message k (Command _ entry)) = do
    des <- use $ replica . decisions
    sOut <- use $ replica . slotOut
    if any (\s -> (s, req) `S.member` des) [1 .. sOut - 1]
        then replica . slotOut += 1
        else do
            response <- applyCommand entry
            replica . slotOut += 1
            writeMsg' k response

replicaOnRequest :: ClientRequest -> ServerM ()
replicaOnRequest c = do
    replica . requests %= S.insert c
    replicaPropose

replicaOnDecision :: Message Decision -> ServerM ()
replicaOnDecision (Message _ (Decision s c)) = do
    replica . decisions %= S.insert (s,c)
    des <- uses (replica . decisions) S.toList
    props <- uses (replica . proposals) S.toList
    sOut <- use $ replica . slotOut
    forM_ [ (d,p) | d@(s1,_) <- des
                  , p@(s2,_) <- props
                  , s1 == s2 && s1 == sOut] $
          \((_,c'),(_,c'')) -> do
              replica . proposals %= S.delete (sOut,c'')
              when (c' /= c'') $ replica . requests %= S.insert c''
    forM_ [d | d@(s1,_) <- des, s1 == sOut] $ replicaPerform . snd

--------- ACCEPTOR ---------

acceptorOnPC :: Message PhaseCommitA -> ServerM ()
acceptorOnPC (Message from (P1A ψ b)) = do
    chosenB <- acceptor . aBallotNum <%= max b
    acc <- use $ acceptor . accepted
    writeMsg' from $ P1B ψ chosenB acc
acceptorOnPC (Message from (P2A ψ pv@(b,_,_))) = do
    b' <- use $ acceptor . aBallotNum
    when (b' == b) $ acceptor . accepted %= S.insert pv
    writeMsg' from $ P2B ψ b'


--------- SCOUT & COMMANDER ---------

spawnScout :: Ballot -> ServerM ()
spawnScout b = do
    ψ <- use $ leader . lUniqueId
    leader . lUniqueId += 1
    leader . lScouts %= M.insert ψ (S.empty, S.empty, b)
    nodes <- serverPeers <$> ask
    forM_ nodes $ \α -> writeMsg' α $ P1A ψ b

spawnCommander :: PValue -> ServerM ()
spawnCommander pval = do
    ψ <- use $ leader . lUniqueId
    leader . lUniqueId += 1
    leader .lCommanders %= M.insert ψ (S.empty, pval)
    nodes <- serverPeers <$> ask
    forM_ nodes $ \α -> writeMsg' α $ P2A ψ pval

sendBack x = do
    self <- serverPid <$> ask
    writeMsg' self x

-- actually, that's scout & commander
leaderOnCommit :: Message PhaseCommitB -> ServerM ()
leaderOnCommit (Message α (P1B ψ b' pvals)) = do
    n <- serverNodesN <$> ask
    scouts <- use $ leader . lScouts
    let suicide = leader . lScouts %= M.delete ψ
    when (ψ `M.member` scouts) $ do
        (_,_,b) <- uses (leader . lScouts) $ fromJust . M.lookup ψ
        if b' == b
        then do (leader . lScouts) %= (M.adjust (\(x,s,y) -> (x,S.union s pvals,y)) ψ)
                (leader . lScouts) %= (M.adjust (\(s,x,y) -> (S.insert α s,x,y)) ψ)
                (answers,pvalues,_) <- uses (leader . lScouts) $ fromJust . M.lookup ψ
                when (length answers > n `div` 2) $ do
                    sendBack $ Adopted b $ S.toList pvalues
                    suicide
        else do sendBack $ Preempted b'
                suicide
leaderOnCommit (Message α (P2B ψ b')) = do
    n <- serverNodesN <$> ask
    commanders <- use $ leader . lCommanders
    let suicide = leader . lCommanders %= M.delete ψ
    when (ψ `M.member` commanders) $ do
        (_,(b,s,c)) <- uses (leader . lCommanders) $ fromJust . M.lookup ψ
        if b' == b
        then do (leader . lCommanders) %= (M.adjust (\(ans,y) -> (S.insert α ans,y)) ψ)
                (answers,_) <- uses (leader . lCommanders) $ fromJust . M.lookup ψ
                when (length answers > n `div` 2) $ do
                    nodes <- serverPeers <$> ask
                    forM_ nodes $ \replica' -> writeMsg' replica' $ Decision s c
                    suicide
        else do sendBack $ Preempted b'
                suicide


--------- LEADER ---------


initLeader :: ServerM ()
initLeader = spawnScout 0

leaderOnNotification :: Message LeaderNotification -> ServerM ()
leaderOnNotification (Message _ (ProposeRequest s c)) = do
    slotTaken <- uses (leader . lProposals) $ any ((== s) . fst) . S.toList
    unless slotTaken $ do
        leader . lProposals %= S.insert (s,c)
        act <- use $ leader . lActive
        blt <- use $ leader . lBallotNum
        when act $ spawnCommander (blt,s,c)
leaderOnNotification (Message _ (Adopted b pvals)) = do
    let pmax = [(s,c) | (b,s,c) <- pvals, all (\(b',s',_) -> s' == s && b' <= b) pvals]
        x <| y = y ++ [(s,c) | (s,c) <- x, all (\(s',c') -> s' /= s) y]
    props <- leader . lProposals <%= (\p -> S.fromList $ (S.toList p) <| pmax)
    ballotN <- use $ leader . lBallotNum
    forM_ props $ \(s,c) -> spawnCommander (ballotN,s,c)
    leader . lActive .= True
               -- (\(_,s,c) -> (s,c)) $ maximumBy (comparing (\(_,s,_) -> s)) pvals
leaderOnNotification (Message from (Preempted ballot)) = do
    blt <- use $ leader . lBallotNum
    self <- serverPid <$> ask
    when ((ballot,from) > (blt,self)) $ do
        leader . lActive .= False
        leader . lBallotNum .= (ballot + 1)
        spawnScout (ballot + 1)
