module Handler.Twenty48 where

import Import

getTwenty48R :: Handler Html
getTwenty48R = do
  sendFile typeHtml "static/2048/index.html"

