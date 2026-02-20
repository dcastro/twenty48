{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Handler.AutoPlay where

import Control.Monad.Extra (whenJust)
import Data.Aeson as J
import Data.Aeson.TH (deriveToJSON)
import Game.AlphaBeta
import Game.Optimized.Board
import Game.Optimized.Moves
import Game.Types
import Import
import Utils.Control (whenJust')
import Utils.Misc (toLazyMaybe)
import Yesod.WebSockets hiding (race_)

data Start = Start Board

data Stop = Stop

instance FromJSON Stop where
  parseJSON = J.withText "Stop" $ \case
    "stop" -> pure Stop
    _ -> empty

instance FromJSON Start where
  parseJSON = map Start . parseJSON

data OutMsg
  = PlayPlayerMsg {_direction :: Maybe Direction}
  | PlayComputerMsg {_coord :: Coord, _cell :: Cell}
  deriving stock (Generic)

$(deriveToJSON defaultOptions ''OutMsg)

postAutoPlayOnceR :: Handler Value
postAutoPlayOnceR = do
  board <- requireInsecureJsonBody
  depth <- aiDepth . appSettings <$> getYesod
  returnJson . PlayPlayerMsg . fmap unPlayer . toLazyMaybe $ alphaBeta board depth

getAutoPlayR :: Handler ()
getAutoPlayR = do
  depth <- aiDepth . appSettings <$> getYesod
  webSockets $ do
    Start board <- receiveJson
    race_
      (autoPlay board depth)
      (receiveJson @_ @Stop)

autoPlay :: Board -> Int -> WebSocketsT Handler ()
autoPlay board depth =
  whenJust' (alphaBeta board depth) $ \player@(Player direction) -> do
    sendJson $ PlayPlayerMsg $ Just direction

    let newBoard = playPlayer player board

    mbComputer <- liftIO $ randomComputerMove newBoard
    whenJust mbComputer $ \computer@(Computer coord cell) -> do
      sendJson $ PlayComputerMsg coord cell

      let newBoard' = playComputer computer newBoard
      autoPlay newBoard' depth

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
