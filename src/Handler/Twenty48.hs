{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Handler.Twenty48 where

import Import
import Yesod.WebSockets hiding (race_)
import qualified Data.Aeson       as J
import           Data.Aeson.TH    (deriveFromJSON, deriveToJSON, defaultOptions)
import           Game.Types
import           Game.Twenty48
import           Game.Ai
import           Data.Alternated as A
import           Utils.Control

getTwenty48R :: Handler Html
getTwenty48R = do
  webSockets wsApp
  sendFile typeHtml "static/2048/index.html"

aiDepth :: Int
aiDepth = 5

wsApp :: WebSocketsT Handler ()
wsApp = 
  forever $ do
    msg <- receiveMsg
    case msg of
      AutoPlayOnceMsg b -> do
        let Path turns _ = maximumBy (comparing score) $ maximize $ map boardEval $ pruneHeight aiDepth $ unfoldPlayerTree b
        whenJust (A.head turns) $ \player -> 
          sendMsg $ playPlayerMsg player
      AutoPlayMsg b -> do
        race_
          (autoPlay b)
          (whileM ((/= StopMsg) <$> receiveMsg))
      StopMsg -> $logError "Stop message received at unexpected time"
      
autoPlay :: Board -> WebSocketsT Handler ()
autoPlay board = do
  let Path turns _ = maximumBy (comparing score) $ maximize $ map boardEval $ pruneHeight aiDepth $ unfoldPlayerTree board
  whenJust (A.head turns) $ \player -> do
    sendMsg $ playPlayerMsg player

    let newBoard = playPlayer player board

    mbComputer <- liftIO $ randomComputerMove newBoard
    whenJust mbComputer $ \computer -> do
      sendMsg $ playComputerMsg computer

      let newBoard' = playComputer computer newBoard
      autoPlay newBoard'

data OutMsg
  = PlayPlayerMsg { direction :: Direction }
  | PlayComputerMsg { coord :: Coord, cell :: Cell }
  deriving (Generic)

playPlayerMsg :: Player -> OutMsg
playPlayerMsg (Player direction) = PlayPlayerMsg direction

playComputerMsg :: Computer -> OutMsg
playComputerMsg (Computer coord cell) = PlayComputerMsg coord cell

data InMsg
  = AutoPlayOnceMsg { board :: Board }
  | AutoPlayMsg     { board :: Board }
  | StopMsg
  deriving (Generic, Show, Eq)

receiveMsg :: (MonadIO m, MonadLogger m) => WebSocketsT m InMsg
receiveMsg = do
  d <- receiveData
  case J.decodeStrict' d of
    Just msg -> pure msg
    Nothing -> do
      $logError $ "Unexpected message received " <> decodeUtf8 d
      receiveMsg

sendMsg :: (MonadIO m, MonadLogger m) => OutMsg -> WebSocketsT m ()
sendMsg = sendTextData . J.encode

$(deriveToJSON defaultOptions ''OutMsg)
$(deriveFromJSON defaultOptions ''InMsg)



