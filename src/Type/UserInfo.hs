module Type.UserInfo where

import MyPrelude

import qualified Type.User as User

data UserInfo = UserInfo
  { name :: !User.Name
  } deriving (Generic, Show)

instance JSONSchema UserInfo where schema    = gSchema
instance FromJSON   UserInfo where parseJSON = gparseJson
instance ToJSON     UserInfo where toJSON    = gtoJson
