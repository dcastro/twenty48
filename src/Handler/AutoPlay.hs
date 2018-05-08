{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}

module Handler.AutoPlay where

import           Control.Monad.Extra  (whenJust)
import           Data.Aeson           as J
import           Data.Aeson.TH        (defaultOptions, deriveToJSON)
import           Game.AlphaBeta
import           Game.Optimized.Board
import           Game.Optimized.Moves
import           Game.Types
import           Import
import           Utils.Control        (whenJust')
import           Utils.Misc           (toLazyMaybe)
import           Yesod.WebSockets     hiding (race_)

aiDepth :: Int
aiDepth = 6

postAutoPlayOnceR :: Handler Value
postAutoPlayOnceR = do
  board <- requireJsonBody
  returnJson . PlayPlayerMsg . fmap unPlayer . toLazyMaybe $ alphaBeta board aiDepth

getAutoPlayR :: Handler ()
getAutoPlayR =
  webSockets $ do
    Start board <- receiveJson
    race_
      (autoPlay board)
      (receiveJson @_ @Stop)

autoPlay :: Board -> WebSocketsT Handler ()
autoPlay board =
  whenJust' (alphaBeta board aiDepth) $ \player@(Player direction) -> do
    sendJson $ PlayPlayerMsg $ Just direction

    let newBoard = playPlayer player board

    mbComputer <- liftIO $ randomComputerMove newBoard
    whenJust mbComputer $ \computer@(Computer coord cell) -> do
      sendJson $ PlayComputerMsg coord cell

      let newBoard' = playComputer computer newBoard
      autoPlay newBoard'

receiveJson :: (MonadIO m, MonadLogger m, FromJSON msg) => WebSocketsT m msg
receiveJson = do
  d <- receiveData
  case J.decodeStrict' d of
    Just msg -> pure msg
    Nothing -> do
      $logError $ "Unexpected message received: " <> decodeUtf8 d
      receiveJson
      
sendJson :: (MonadIO m, ToJSON msg) => msg -> WebSocketsT m ()
sendJson = sendTextData . J.encode

data Start = Start Board
data Stop = Stop

instance FromJSON Stop where
  parseJSON = J.withText "Stop" $ \case
    "stop"  -> pure Stop
    _       -> empty

instance FromJSON Start where
  parseJSON = map Start . parseJSON

data OutMsg
  = PlayPlayerMsg { _direction :: Maybe Direction }
  | PlayComputerMsg { _coord :: Coord, _cell :: Cell }
  deriving (Generic)

$(deriveToJSON defaultOptions ''OutMsg)
