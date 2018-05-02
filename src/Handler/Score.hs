{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}

module Handler.Score where

import           Data.Aeson.TH      (defaultOptions, deriveFromJSON,
                                     deriveToJSON)
import           Database.Esqueleto hiding (Value)
import           Import             hiding (on, (==.))

data UserWithScore = UserWithScore
  { email :: Text
  , name  :: Text
  , score :: Int
  } deriving (Generic)

data Scores = Scores
  { myScores  :: Maybe [GameScore]
  , allScores :: [GameScore]
  } deriving (Generic)

$(deriveFromJSON defaultOptions ''UserWithScore)
$(deriveToJSON defaultOptions ''Scores)

postScoreR :: Handler ()
postScoreR = do
  UserWithScore {..} <- requireJsonBody
  runDB $ do
    insertedEmail <- upsert (User email) []
    insert_ $ GameScore name score (entityKey insertedEmail)

getScoresR :: Handler Value
getScoresR = do
  emailMb <- lookupGetParam "user"
  myScores <-
    forM emailMb $ \thisEmail ->
      runDB $
      select $
      from $ \(user `InnerJoin` score) -> do
        on (user ^. UserId ==. score ^. GameScoreUserId)
        where_ (user ^. UserEmail ==. val thisEmail)
        orderBy [desc (score ^. GameScoreScore)]
        limit 10
        pure score
  allScores <-
    runDB $
    select $
    from $ \score -> do
      orderBy [desc (score ^. GameScoreScore)]
      limit 10
      pure score
  returnJson $ Scores (fmap entityVal <$> myScores) (entityVal <$> allScores)
