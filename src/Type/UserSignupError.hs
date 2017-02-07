module Type.UserSignupError where

import MyPrelude

import Servant

import Type.ToServantErr (ToServantErr (..))

data UserSignupError
  = InvalidPassword
  | InvalidUserName
  deriving (Eq, Generic, Ord, Show)

instance JSONSchema UserSignupError where
  schema = gSchema
instance FromJSON UserSignupError where
  parseJSON = gparseJson
instance ToJSON UserSignupError where
  toJSON = gtoJson
instance ToServantErr UserSignupError where
  toServantErr _ = err400
