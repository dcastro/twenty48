{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Handler.Twenty48 where

import Import
import Yesod.WebSockets
import qualified Data.Aeson       as J
import           Data.Aeson.TH    (deriveJSON, defaultOptions)
import           Twenty48.Types
import           Twenty48.Twenty48

getTwenty48R :: Handler Html
getTwenty48R = do
  webSockets wsApp
  sendFile typeHtml "static/2048/index.html"

wsApp :: WebSocketsT Handler ()
wsApp = 
  forever $ do
    msg <- J.decodeStrict' <$> receiveData
    case msg of
      Just (AutoPlayOnceMsg board)  -> sendTextData $ J.encode $ MoveMsg U
      _                             -> error "Unexpected message received"

data OutMsg
  = MoveMsg { direction :: Direction }
  | OtherOut
  deriving (Generic)

data InMsg =
  AutoPlayOnceMsg { board :: Board }
  | OtherIn
  deriving (Generic, Show)

$(deriveJSON defaultOptions ''OutMsg)
$(deriveJSON defaultOptions ''InMsg)


