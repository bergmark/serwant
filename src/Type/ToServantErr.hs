module Type.ToServantErr (ToServantErr (..)) where

import MyPrelude

import Servant (ServantErr)

class ToServantErr a where
  toServantErr :: a -> ServantErr

instance ToServantErr Void where
  toServantErr = absurd
