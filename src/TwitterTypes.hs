{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module TwitterTypes where
import Data.Aeson
import Data.Text
import GHC.Generics
import Control.Monad

-- help from: https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/json
data User =
  User { name :: Text
       , screen_name :: Text } deriving (Show, Generic)

instance FromJSON User
instance ToJSON User

data Media =
  Media { media_type :: Text
        , media_url :: Text
        , media_url_https :: Text } deriving (Show, Generic)

-- type field clashes w/ Haskell keyword "type"
instance FromJSON Media where
  parseJSON (Object o) =
    Media <$> o .: "type"
    <*> o .: "media_url"
    <*> o .: "media_url_https"
  parseJSON _ = mzero

instance ToJSON Media where
  toJSON (Media m_type m_url m_url_https) =
    object [ "type" .= m_type
           , "media_url" .= m_url
           , "media_url_https" .= m_url_https
           ]

data Entity =
  Entity { media :: Maybe [Media] } deriving (Show, Generic)

instance FromJSON Entity where
  parseJSON (Object o) =
    Entity <$> o .:? "media"
  parseJSON _ = mzero

instance ToJSON Entity where
  toJSON (Entity med) =
    case med of
      Just m -> object [ "media" .= m ]
      Nothing -> object []

data Status =
  Status { text :: Text
         , id_str :: Text
         , user :: User
         , entities :: Entity
         , created_at :: Text } deriving (Show, Generic)

instance FromJSON Status
instance ToJSON Status

data TwitterSearch =
  TwitterSearch { statuses :: [Status]
                } deriving (Show, Generic)

instance FromJSON TwitterSearch
instance ToJSON TwitterSearch
