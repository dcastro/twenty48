module Handler.Twenty48 where

import           Import

getTwenty48R :: Handler Html
getTwenty48R =
  sendFile typeHtml "static/2048/index.html"
