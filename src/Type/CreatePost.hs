module Type.CreatePost where

import MyPrelude

type Title = StrictText

data CreatePost = CreatePost
  { title :: !Title
  , content :: !StrictText
  } deriving (Eq, Generic, Ord, Show)

instance JSONSchema CreatePost where
  schema = gSchema
instance FromJSON CreatePost where
  parseJSON = gparseJson
instance ToJSON CreatePost where
  toJSON = gtoJson
