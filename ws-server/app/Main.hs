{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent (MVar, modifyMVar_, newMVar, readMVar)
import           Control.Monad      (forever)
import qualified Network.WebSockets as WS

data State = State
  { drawer  :: Maybe WS.Connection
  , guesser :: Maybe WS.Connection
  }

newState :: State
newState = State { drawer = Nothing, guesser = Nothing  }

isStateReady :: State -> Bool
isStateReady (State{ drawer = Just _, guesser = Just _ }) = True
isStateReady _                                            = False

addPlayer :: WS.Connection -> WS.DataMessage -> State -> State
addPlayer conn (WS.Text "drawer" _) state  = state { drawer = Just conn }
addPlayer conn (WS.Text "guesser" _) state = state { guesser = Just conn }
addPlayer conn _ state                     = state

application :: MVar State -> WS.ServerApp
application varState pending = do
  conn <- WS.acceptRequest pending
  forever $ do
    dm <- WS.receiveDataMessage conn
    print dm
    state <- readMVar varState
    modifyMVar_ varState $ \state ->
      case state of
        State { drawer = Just dConn, guesser = Just gConn } -> do
            dm <- WS.receiveDataMessage dConn
            WS.sendDataMessage gConn dm
            return state
        _ -> return $ addPlayer conn dm state

main :: IO ()
main = do
  state <- newMVar newState
  WS.runServer "127.0.0.1" 9160 $ application state
