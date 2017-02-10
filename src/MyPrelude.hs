module MyPrelude
  ( module X
  ) where

import Control.Concurrent.STM as X (STM, TVar, atomically, modifyTVar, newTVarIO, readTVar)
import Control.Monad as X (unless)
import Control.Monad.Except as X (ExceptT (ExceptT), MonadError, throwError)
import Control.Monad.Reader as X (MonadReader, ReaderT, asks, runReaderT)
import Control.Monad.State as X (MonadState, StateT, gets)
import Control.Monad.Trans as X (MonadIO (liftIO), lift)
import Data.Aeson as X (ToJSON (..), FromJSON (..), encode)
import Data.Foldable as X (elem)
import Data.HashMap.Strict as X (HashMap)
import Data.JSON.Schema as X (JSONSchema (..), gSchema)
import Data.List as X (sortBy)
import Data.Ord as X (comparing)
import Data.Set as X (Set)
import Data.String.Conversions as X (StrictText, cs)
import Data.Time as X (UTCTime, getCurrentTime)
import Data.Void as X (Void)
import GHC.Generics as X (Generic)
import Generics.Generic.Aeson as X (gparseJson, gtoJson)
import Prelude.Compat as X
       (Applicative(..), Bool(..), Int, Either(..), Eq(..), Functor(..),
        IO, Maybe(..), Monad((>>=)), Num(..), Ord(..), Show(show), Word,
        (.), (&&), (<$>), ($), (=<<), drop, error, flip, fromIntegral, id,
        maybe, not, take, undefined)
import Safe as X (headMay, readNote)
