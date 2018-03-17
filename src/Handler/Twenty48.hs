{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Handler.Twenty48 where

import Import
import Yesod.WebSockets
import qualified Data.Aeson       as J
import           Data.Aeson.TH    (deriveFromJSON, deriveToJSON, defaultOptions)
import           Twenty48.Types
import           Twenty48.Twenty48
import           Twenty48.Ai
import           Data.Alternated

getTwenty48R :: Handler Html
getTwenty48R = do
  webSockets wsApp
  sendFile typeHtml "static/2048/index.html"

aiDepth :: Int
aiDepth = 5

wsApp :: WebSocketsT Handler ()
wsApp = 
  forever $ do
    msg <- J.decodeStrict' <$> receiveData
    case msg of
      Just (AutoPlayOnceMsg b)  ->
        let Path (Alternated (Player dir) _) _ = maximumBy (comparing score) $ maximize $ map boardEval $ pruneHeight aiDepth $ unfoldPlayerTree b
        in  sendTextData $ J.encode $ PlayPlayer dir
      _                         -> error "Unexpected message received"

data OutMsg
  = PlayPlayer { direction :: Direction }
  | PlayerComputer { coord :: Coord, cell :: Cell }
  deriving (Generic)

data InMsg
  = AutoPlayOnceMsg { board :: Board }
  | OtherIn
  deriving (Generic, Show)

$(deriveToJSON defaultOptions ''OutMsg)
$(deriveFromJSON defaultOptions ''InMsg)


