{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Handler.Twenty48 where

import           Control.Monad.Extra  (whenJust)
import qualified Data.Aeson           as J
import           Data.Aeson.TH        (defaultOptions, deriveFromJSON,
                                       deriveToJSON)
import qualified Data.Strict.Maybe    as M
import           Game.AlphaBeta
import           Game.Optimized.Board
import           Game.Optimized.Moves
import           Game.Types
import           Import
import           Utils.Control
import           Yesod.WebSockets     hiding (race_)

getTwenty48R :: Handler Html
getTwenty48R = do
  webSockets wsApp
  sendFile typeHtml "static/2048/index.html"

aiDepth :: Int
aiDepth = 6

wsApp :: WebSocketsT Handler ()
wsApp = do
  producing <- newIORef False
  forever $ do
    msg <- receiveMsg
    case msg of
      AutoPlayOnceMsg board -> do
        whenJust' (alphaBeta board aiDepth) $ \player -> 
          sendMsg $ playPlayerMsg player
      AutoPlayMsg b -> do
        whenM (not <$> readIORef producing) $ do
          atomicWriteIORef producing True
          void . async $ autoPlay producing b
      StopMsg -> atomicWriteIORef producing False
      
autoPlay :: IORef Bool -> Board -> WebSocketsT Handler ()
autoPlay producing board = do
  whenM (readIORef producing) $
    case alphaBeta board aiDepth of
      M.Nothing -> atomicWriteIORef producing False
      M.Just player -> do
        sendMsg $ playPlayerMsg player

        let newBoard = playPlayer player board

        mbComputer <- liftIO $ randomComputerMove newBoard
        whenJust mbComputer $ \computer -> do
          sendMsg $ playComputerMsg computer

          let newBoard' = playComputer computer newBoard
          autoPlay producing newBoard'

data OutMsg
  = PlayPlayerMsg { _direction :: Direction }
  | PlayComputerMsg { _coord :: Coord, _cell :: Cell }
  deriving (Generic)

playPlayerMsg :: Player -> OutMsg
playPlayerMsg (Player direction) = PlayPlayerMsg direction

playComputerMsg :: Computer -> OutMsg
playComputerMsg (Computer coord cell) = PlayComputerMsg coord cell

sendMsg :: MonadIO m => OutMsg -> WebSocketsT m ()
sendMsg = sendTextData . J.encode


data InMsg
  = AutoPlayOnceMsg { _board :: Board }
  | AutoPlayMsg     { _board :: Board }
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

$(deriveToJSON defaultOptions ''OutMsg)
$(deriveFromJSON defaultOptions ''InMsg)

