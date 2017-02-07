module Type.User where

import MyPrelude

type Name = StrictText
type Password = StrictText

data User = User
  { name     :: !Name
  , password :: !Password
  } deriving (Eq, Generic, Ord, Show)

instance JSONSchema User where schema    = gSchema
instance FromJSON   User where parseJSON = gparseJson
instance ToJSON     User where toJSON    = gtoJson
-- We might want to skip the ToJSON instance so we don't accidentally
-- serve passwords, but this type is accepted on signup which means a
-- haskell client needs to be able to serialize it.
