{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Score where

import Import
import Data.Aeson.TH (deriveFromJSON, defaultOptions)

data UserWithScore = UserWithScore
  { email :: Text
  , name  :: Text
  , score :: Int
  }
  deriving (Generic)

$(deriveFromJSON defaultOptions ''UserWithScore)

postScoreR :: Handler ()
postScoreR = do
  UserWithScore{..} <- requireJsonBody
  runDB $ do
    insertedEmail <- upsert (User email) []
    insert_ $ GameScore name score (entityKey insertedEmail)

  