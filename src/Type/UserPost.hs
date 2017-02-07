module Type.UserPost where

import MyPrelude

import Type.CreatePost (CreatePost)
import Type.User (User)

data UserPost = UserPost
  { user :: User
  , post :: CreatePost
  } deriving (Eq, Generic, Ord, Show)

instance JSONSchema UserPost where schema    = gSchema
instance FromJSON   UserPost where parseJSON = gparseJson
instance ToJSON     UserPost where toJSON    = gtoJson
