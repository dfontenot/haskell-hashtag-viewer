{-# LANGUAGE DeriveGeneric #-}

module TwitterTypes where
import Data.Aeson
import Data.Text
import GHC.Generics

data Status =
  Status { text :: Text
         , created_at :: Text } deriving (Show, Generic)

instance FromJSON Status
instance ToJSON Status

data TwitterSearch =
  TwitterSearch { statuses :: [Status]
                } deriving (Show, Generic)

instance FromJSON TwitterSearch
instance ToJSON TwitterSearch
