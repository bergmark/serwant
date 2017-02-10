module Type.Post where

import MyPrelude

import Data.Time (UTCTime)

import qualified Type.User as User

type Id = Word
type Title = StrictText

data Post = Post
  { id          :: Id
  , author      :: User.Name
  , createdTime :: UTCTime
  , title       :: Title
  , content     :: StrictText
  } deriving (Eq, Generic, Ord, Show)

instance JSONSchema Post where schema    = gSchema
instance FromJSON   Post where parseJSON = gparseJson
instance ToJSON     Post where toJSON    = gtoJson
