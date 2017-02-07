module Type.PostError where

import MyPrelude

import Servant

import Type.ToServantErr

data PostError
  = InvalidTitle
  | InvalidContent
  deriving (Eq, Generic, Ord, Show)

instance FromJSON   PostError where parseJSON = gparseJson
instance JSONSchema PostError where schema    = gSchema
instance ToJSON     PostError where toJSON    = gtoJson

instance ToServantErr PostError where
  toServantErr _ = err400
