{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Twenty48 where

import Import
import Yesod.WebSockets
import qualified Data.Aeson       as A
import qualified Data.Aeson.Text  as A
import Twenty48.Types

getTwenty48R :: Handler Html
getTwenty48R = do
  webSockets wsApp
  sendFile typeHtml "static/2048/index.html"

wsApp :: WebSocketsT Handler ()
wsApp = 
  forever $ do
    board <- fromMaybe (error "Unexpected data") . A.decodeStrict' @Board <$> receiveData
    $logDebug $ "Received: " <> toStrict (A.encodeToLazyText board)
