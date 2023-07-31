module MineHraft.Session.Store where

import MineHraft.Session (HasSession (sessionName), Session, newSession)
import Network.Socket (SockAddr)
import Polysemy
import Polysemy.State
import Relude.Extra
import Prelude hiding (State, gets, modify)

type SessionStore = Map SockAddr Session

getSession :: Member (State SessionStore) r => SockAddr -> Sem r (Maybe Session)
getSession addr = gets (lookup addr)

getOrCreateSession :: Member (State SessionStore) r => SockAddr -> Sem r Session
getOrCreateSession addr = do
    maybeSession <- getSession addr
    case maybeSession of
        Just session -> pure session
        Nothing -> do
            let session = newSession addr
            modify (insert addr session)
            pure session

updateSession :: Member (State SessionStore) r => SockAddr -> (Session -> Session) -> Sem r ()
updateSession addr f = do
    maybeSession <- getSession addr
    case maybeSession of
        Just session -> modify (insert addr (f session))
        Nothing -> pass

putSession :: Member (State SessionStore) r => Session -> Sem r ()
putSession session = modify (insert (session ^. sessionName) session)