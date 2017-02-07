module Type.Reason (Reason (..)) where

import MyPrelude

import Servant

import Type.ToServantErr

data Reason e
  = BadRequest
  | DomainReason e
  | NotAllowed
  | NotFound
  deriving (Generic, Show)

instance ToServantErr e => ToServantErr (Reason e) where
  toServantErr = \case
    BadRequest     -> err400
    DomainReason e -> toServantErr e
    NotAllowed     -> err403
    NotFound       -> err404

instance ToJSON a => ToJSON (Reason a) where
  toJSON = gtoJson

instance FromJSON a => FromJSON (Reason a) where
  parseJSON = gparseJson
