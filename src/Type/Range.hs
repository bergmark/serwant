module Type.Range (Range (..)) where

import MyPrelude

data Range = Range
  { offset :: !Int
  , count :: !Int
  } deriving (Eq, Show)
