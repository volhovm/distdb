-- | Logic for paxos expressed in the ServerM monad terms
module PaxosLogic
       ( replicaOnRequest
       , replicaOnDecision
       , acceptorOnPC
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
import           PaxosTypes               (Ballot, ClientRequest, Decision (..),
                                           LeaderNotification (..), PValue,
                                           PhaseCommitA (..), PhaseCommitB (..), Slot,
                                           aBallotNum, accepted, decisions, lActive,
                                           lBallotNum, lCommanders, lProposals, lScouts,
                                           lUniqueId, proposals, requests, slotIn,
                                           slotOut)
import           ServerTypes              (ServerM, acceptor, acceptorsN, hashmap,
                                           knownAcceptors, knownLeaders, knownReplicas,
                                           leader, replica, serverPid, writeLog,
                                           writeMsg')
import           Types                    (Command (..), Entry (..), EntryRequest (..),
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
    writeLog "Replica on propose called"
    sIn <- use $ replica . slotIn
    sOut <- use $ replica . slotOut
    reqs <- uses (replica . requests) S.toList
    decs <- use $ replica .decisions
    when (sIn < sOut + _WINDOW && not (null reqs)) $ do
        let c = head reqs
        unless (any (\(s,_) -> s == sIn) decs) $ do
            replica . requests %= S.delete c
            replica . proposals %= S.insert (sIn, c)
            nodes <- knownLeaders <$> ask
            writeLog "Replica sending leaders proposeRequests"
            forM_ nodes $ \leader -> writeMsg' leader $ ProposeRequest sIn c
        replica . slotIn += 1
        replicaPropose

replicaPerform :: ClientRequest -> ServerM ()
replicaPerform req@(Message k (Command _ entry)) = do
    writeLog "Replica on perform called"
    des <- use $ replica . decisions
    sOut <- use $ replica . slotOut
    if any (\s -> (s, req) `S.member` des) [1 .. sOut - 1]
        then replica . slotOut += 1
        else do
            response <- applyCommand entry
            replica . slotOut += 1
            writeLog "Replica sending response"
            writeMsg' k response

replicaOnRequest :: ClientRequest -> ServerM ()
replicaOnRequest c = do
    writeLog $ "Replica on request called for request: " ++ show c
    replica . requests %= S.insert c
    replicaPropose

replicaOnDecision :: Message Decision -> ServerM ()
replicaOnDecision (Message _ d@(Decision s c)) = do
    writeLog $ "Replica on decision called for decision: " ++ show d
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
acceptorOnPC (Message from p@(P1A ψ b)) = do
    writeLog $ "Acceptor: got p1a request: " ++ show p
    chosenB <- acceptor . aBallotNum <%= max b
    acc <- use $ acceptor . accepted
    let response = P1B ψ chosenB acc
    writeLog $ "Acceptor: responding with " ++ show response
    writeMsg' from response
acceptorOnPC (Message from p@(P2A ψ pv@(b,_,_))) = do
    writeLog $ "Acceptor: got p2a request: " ++ show p
    b' <- use $ acceptor . aBallotNum
    when (b' == b) $ acceptor . accepted %= S.insert pv
    let response = P2B ψ b'
    writeLog $ "Acceptor: responding with " ++ show response
    writeMsg' from response


--------- SCOUT & COMMANDER ---------

spawnScout :: Ballot -> ServerM ()
spawnScout b = do
    ψ <- use $ leader . lUniqueId
    writeLog $ "Spawning scout #" ++ show ψ
    leader . lUniqueId += 1
    leader . lScouts %= M.insert ψ (S.empty, S.empty, b)
    nodes <- knownAcceptors <$> ask
    writeLog $ "Scout #" ++ show ψ ++ ": sending P1A to acceptors"
    forM_ nodes $ \α -> writeMsg' α $ P1A ψ b
    writeLog $ "Scout #" ++ show ψ ++ ": initalization finished"

spawnCommander :: PValue -> ServerM ()
spawnCommander pval = do
    ψ <- use $ leader . lUniqueId
    writeLog $ "Spawning commander #" ++ show ψ
    leader . lUniqueId += 1
    leader .lCommanders %= M.insert ψ (S.empty, pval)
    nodes <- knownAcceptors <$> ask
    writeLog $ "Commander #" ++ show ψ ++ ": sending P2A to acceptors"
    forM_ nodes $ \α -> writeMsg' α $ P2A ψ pval
    writeLog $ "Commander #" ++ show ψ ++ ": initalization finished"

sendBack x = do
    self <- serverPid <$> ask
    writeMsg' self x

-- actually, that's scout & commander
leaderOnCommit :: Message PhaseCommitB -> ServerM ()
leaderOnCommit (Message α (P1B ψ b' pvals)) = do
    writeLog $ "Scout request for #" ++ show ψ
    n <- acceptorsN <$> ask
    scouts <- use $ leader . lScouts
    let suicide = leader . lScouts %= M.delete ψ
    when (ψ `M.member` scouts) $ do
        writeLog "Scout request will be proceeded"
        (_,_,b) <- uses (leader . lScouts) $ fromJust . M.lookup ψ
        if b' == b
        then do (leader . lScouts) %= (M.adjust (\(x,s,y) -> (x,S.union s pvals,y)) ψ)
                (leader . lScouts) %= (M.adjust (\(s,x,y) -> (S.insert α s,x,y)) ψ)
                (answers,pvalues,_) <- uses (leader . lScouts) $ fromJust . M.lookup ψ
                when (length answers > n `div` 2) $ do
                    writeLog "Scout: More than half of responses, exiting, sending Adopted"
                    sendBack $ Adopted b $ S.toList pvalues
                    suicide
        else do writeLog "Scout: Sending Preempted"
                sendBack $ Preempted b'
                suicide
leaderOnCommit (Message α (P2B ψ b')) = do
    writeLog $ "Commander request for #" ++ show ψ
    n <- acceptorsN <$> ask
    commanders <- use $ leader . lCommanders
    let suicide = leader . lCommanders %= M.delete ψ
    when (ψ `M.member` commanders) $ do
        writeLog "Commander request will be proceeded"
        (_,(b,s,c)) <- uses (leader . lCommanders) $ fromJust . M.lookup ψ
        if b' == b
        then do writeLog "Commander: adding this response"
                (leader . lCommanders) %= (M.adjust (\(ans,y) -> (S.insert α ans,y)) ψ)
                (answers,_) <- uses (leader . lCommanders) $ fromJust . M.lookup ψ
                when (length answers > n `div` 2) $ do
                    writeLog "Commander: More than half of responses, exiting, sending Decision"
                    nodes <- knownReplicas <$> ask
                    forM_ nodes $ \replica' -> writeMsg' replica' $ Decision s c
                    suicide
        else do writeLog "Commander: sending Preempted"
                sendBack $ Preempted b'
                suicide


--------- LEADER ---------


initLeader :: ServerM ()
initLeader = spawnScout 0

leaderOnNotification :: Message LeaderNotification -> ServerM ()
leaderOnNotification (Message _ pr@(ProposeRequest s c)) = do
    writeLog $ "leaderOnNotification: got ProposeRequest: " ++ show pr
    slotTaken <- uses (leader . lProposals) $ any ((== s) . fst) . S.toList
    unless slotTaken $ do
        writeLog "leaderOnNotification: slot wasn't taken"
        leader . lProposals %= S.insert (s,c)
        act <- use $ leader . lActive
        blt <- use $ leader . lBallotNum
        unless act $ writeLog "inactive, won't spawn commander"
        when act $ do
            writeLog "leaderOnNotification: spawning commander"
            spawnCommander (blt,s,c)
leaderOnNotification (Message _ a@(Adopted b pvals)) = do
    writeLog $ "leaderOnNotification: got adopted: " ++ show a
    let pmax = [(s,c) | (b,s,c) <- pvals, all (\(b',s',_) -> s' == s && b' <= b) pvals]
        x <| y = y ++ [(s,c) | (s,c) <- x, all (\(s',_) -> s' /= s) y]
    props <- leader . lProposals <%= (\p -> S.fromList $ (S.toList p) <| pmax)
    writeLog $ "New props:" ++ show props
    ballotN <- use $ leader . lBallotNum
    writeLog "leaderOnNotification: spawning commanders and activating"
    forM_ props $ \(s,c) -> spawnCommander (ballotN,s,c)
    leader . lActive .= True
               -- (\(_,s,c) -> (s,c)) $ maximumBy (comparing (\(_,s,_) -> s)) pvals
leaderOnNotification (Message from p@(Preempted ballot)) = do
    writeLog $ "leaderOnNotification: got Preempted: " ++ show p
    blt <- use $ leader . lBallotNum
    self <- serverPid <$> ask
    when ((ballot,from) > (blt,self)) $ do
        writeLog "leaderOnNotification: deactivating and launching scout"
        leader . lActive .= False
        leader . lBallotNum .= (ballot + 1)
        spawnScout (ballot + 1)
