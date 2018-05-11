{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.Twenty48 where

import           Import
import           Text.Hamlet           (hamletFile)
import           Text.Julius           (juliusFile)
import           Text.Shakespeare.Sass

getTwenty48R :: Handler Html
getTwenty48R = do
  pc <-
    widgetToPageContent $ do
      traverse_
        toWidget
        [ $(juliusFile "templates/js/bind_polyfill.js")
        , $(juliusFile "templates/js/classlist_polyfill.js")
        , $(juliusFile "templates/js/animframe_polyfill.js")
        , $(juliusFile "templates/js/keyboard_input_manager.js")
        , $(juliusFile "templates/js/html_actuator.js")
        , $(juliusFile "templates/js/login_service.js")
        , $(juliusFile "templates/js/grid.js")
        , $(juliusFile "templates/js/tile.js")
        , $(juliusFile "templates/js/local_storage_manager.js")
        , $(juliusFile "templates/js/game_manager.julius")
        , $(juliusFile "templates/js/application.js")
        ]
      toWidget $(wsass' [] "templates/sass/main.scss")
  withUrlRenderer $(hamletFile "templates/twenty48.hamlet")
