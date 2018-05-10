{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.Twenty48 where

import           Import
import           Text.Hamlet (hamletFile)
import           Text.Julius (juliusFile)

getTwenty48R :: Handler Html
getTwenty48R = do
  pc <-
    widgetToPageContent $
    traverse_
      toWidget
      [ $(juliusFile "static/2048/js/bind_polyfill.js")
      , $(juliusFile "static/2048/js/classlist_polyfill.js")
      , $(juliusFile "static/2048/js/animframe_polyfill.js")
      , $(juliusFile "static/2048/js/keyboard_input_manager.js")
      , $(juliusFile "static/2048/js/html_actuator.js")
      , $(juliusFile "static/2048/js/login_service.js")
      , $(juliusFile "static/2048/js/grid.js")
      , $(juliusFile "static/2048/js/tile.js")
      , $(juliusFile "static/2048/js/local_storage_manager.js")
      , $(juliusFile "static/2048/js/game_manager.js")
      , $(juliusFile "static/2048/js/application.js")
      ]
  withUrlRenderer $(hamletFile "templates/twenty48.hamlet")
