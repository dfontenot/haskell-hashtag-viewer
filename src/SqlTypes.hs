module SqlTypes where

import Data.Text
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

-- help from: https://hackage.haskell.org/package/sqlite-simple-0.4.9.0/docs/Database-SQLite-Simple.html

{- fields:
 - tweet_id (id_str in json result)
 - user_name
 - screen_name
 - text
-}

data TweetFields = TweetFields Text Text Text Text

instance FromRow TweetFields where
  fromRow = TweetFields <$> field <*> field <*> field <*> field

instance ToRow TweetFields where
  toRow (TweetFields tweet_id user_name screen_name tweet) = toRow (tweet_id, user_name, screen_name, tweet)
