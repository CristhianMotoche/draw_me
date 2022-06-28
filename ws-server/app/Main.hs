{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Network.WebSockets as WS

application :: WS.ServerApp
application pending = do
  conn <- WS.acceptRequest pending
  WS.sendDataMessage conn (WS.Text "Hello" Nothing)

main :: IO ()
main = WS.runServer "127.0.0.1" 9160 application
