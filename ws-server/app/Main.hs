{-# LANGUAGE OverloadedStrings #-}

module Main where
import           Control.Concurrent   (MVar, modifyMVar_, newMVar, readMVar)
import           Control.Exception    (catch)
import           Control.Monad        (forever)
import qualified Data.ByteString.Lazy as BL
import           Data.Text            (Text)
import qualified Network.WebSockets   as WS

data State = State
  { drawer  :: Maybe WS.Connection
  , guesser :: Maybe WS.Connection
  }

stateToStr (State { drawer = Just _, guesser = Just _ })   = "Full has both"
stateToStr (State { drawer = Just _, guesser = Nothing })  = "Missing guesser"
stateToStr (State { drawer = Nothing, guesser = Just _ })  = "Missing drawer"
stateToStr (State { drawer = Nothing, guesser = Nothing }) = "Missing both"

newState :: State
newState = State { drawer = Nothing, guesser = Nothing  }

isStateReady :: State -> Bool
isStateReady (State{ drawer = Just _, guesser = Just _ }) = True
isStateReady _                                            = False

addPlayer :: WS.Connection -> WS.DataMessage -> State -> State
addPlayer conn (WS.Text "drawer" _) state  = state { drawer = Just conn }
addPlayer conn (WS.Text "guesser" _) state = state { guesser = Just conn }
addPlayer conn _ state                     = state

data MsgTo = ToGuesser | ToDrawer

parseMsgTo :: WS.DataMessage -> MsgTo
parseMsgTo (WS.Text msg _) =
  if "g:" `BL.isPrefixOf` msg
  then ToDrawer
  else ToGuesser
parseMsgTo _ = ToGuesser


application :: MVar State -> WS.ServerApp
application varState pending = do
  conn <- WS.acceptRequest pending
  forever $ do
    dm <- WS.receiveDataMessage conn
    print dm
    state <- readMVar varState
    print (stateToStr state)
    modifyMVar_ varState $ \state ->
      case state of
        State { drawer = Just dConn, guesser = Just gConn } -> do
            -- dm <- WS.receiveDataMessage dConn
            -- print "2:"
            -- print dm
            case parseMsgTo dm of
              ToDrawer  -> WS.sendDataMessage dConn dm
              ToGuesser -> WS.sendDataMessage gConn dm
            return state
        _ -> return $ addPlayer conn dm state
  `catch` \e -> do
    state <- readMVar varState
    handleClose state e
    nState <- newMVar newState
    application nState pending

handleClose (State { drawer = Just dConn })  WS.ConnectionClosed = WS.sendClose dConn ("" :: Text)
handleClose (State { guesser = Just gConn }) WS.ConnectionClosed  = WS.sendClose gConn ("" :: Text)
handleClose _                     WS.ConnectionClosed = return ()
handleClose _                     _ = return ()

main :: IO ()
main = do
  state <- newMVar newState
  WS.runServer "127.0.0.1" 9160 $ application state
