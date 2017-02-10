module Type.Range
  ( Range (..)
  , list
  ) where

import MyPrelude

data Range = Range
  { offset :: !(Maybe Word)
  , limit :: !(Maybe Word)
  } deriving (Eq, Show)

list :: Range -> [a] -> [a]
list r = lim . off
  where
    off = maybe id (drop . fromIntegral) . offset $ r
    lim = maybe id (take . fromIntegral) . limit $ r
