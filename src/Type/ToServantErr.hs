module Type.ToServantErr (ToServantErr (..)) where

import Servant (ServantErr)

class ToServantErr a where
  toServantErr :: a -> ServantErr
