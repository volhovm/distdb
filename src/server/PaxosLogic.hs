-- | Logic for paxos expressed in the ServerM monad terms
module PaxosLogic
       ( replicaOnRequest
       , replicaOnDecision
       , acceptorOnPC
       , spawnScout
       , spawnCommander
       , initLeader
       ) where

import           Control.Lens             (use, uses, (%=), (+=), (<%=))
import           Control.Monad            (forM_, unless, when)
import           Control.Monad.RWS.Strict (ask)
import qualified Data.Map                 as M
import qualified Data.Set                 as S

import           Communication            (Message (..))
import           PaxosTypes               (Ballot, ClientRequest, Command (..),
                                           Decision (..), LeaderNotification (..), PValue,
                                           PhaseCommitA (..), PhaseCommitB (..), Slot,
                                           aBallotNum, accepted, decisions, lCommanders,
                                           lScouts, proposals, requests, slotIn, slotOut)
import           ServerTypes              (ServerM, acceptor, hashmap, leader, replica,
                                           serverPeers, writeMsg')
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
acceptorOnPC (Message from (P1A λ b)) = do
    chosenB <- acceptor . aBallotNum <%= max b
    acc <- use $ acceptor . accepted
    writeMsg' from $ P1B λ chosenB acc
acceptorOnPC (Message from (P2A λ pv@(b,_,_))) = do
    b' <- use $ acceptor . aBallotNum
    when (b' == b) $ acceptor . accepted %= S.insert pv
    writeMsg' from $ P2B (b',λ)


--------- SCOUT & COMMANDER ---------

spawnScout :: Int -> Ballot -> ServerM ()
spawnScout λ b = do
    leader . lScouts %= M.insert λ (S.empty, [])
    nodes <- serverPeers <$> ask
    forM_ nodes $ \α -> writeMsg' α $ P1A λ b

spawnCommander :: Int -> PValue -> ServerM ()
spawnCommander λ pval = do
    leader .lCommanders %= M.insert λ (S.empty, [])
    nodes <- serverPeers <$> ask
    forM_ nodes $ \α -> writeMsg' α $ P2A λ pval

--------- LEADER ---------

initLeader :: ServerM ()
initLeader = spawnScout 0 0
