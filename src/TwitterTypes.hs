{-# LANGUAGE DeriveGeneric #-}

module TwitterTypes where
import Data.Aeson
import Data.Text
import GHC.Generics

data User =
  User { name :: Text
       , screen_name :: Text } deriving (Show, Generic)

instance FromJSON User
instance ToJSON User

data Status =
  Status { text :: Text
         , id :: Integer
         , user :: User
         , created_at :: Text } deriving (Show, Generic)

instance FromJSON Status
instance ToJSON Status

data TwitterSearch =
  TwitterSearch { statuses :: [Status]
                } deriving (Show, Generic)

instance FromJSON TwitterSearch
instance ToJSON TwitterSearch
