{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ApiTypes where

import MyPrelude

import Servant (Handler, ServantErr)

import Type.User (User)

-- import Type.Comment (Comment)
import Type.Post (Post)
-- import qualified Type.Post as Post


data ServerData = ServerData
  { users    :: !(TVar (Set User))
  , posts    :: TVar (Set Post)
--  , comments :: TVar (HashMap Post.Id (Set Comment))
  }

newtype BlogApi a = BlogApi
  { unBlogApi :: ReaderT ServerData Handler a
  } deriving ( Applicative
             , Functor
             , Monad
             , MonadIO
             , MonadReader ServerData
             , MonadError ServantErr
             )

runBlogApi :: ServerData -> BlogApi a -> Handler a
runBlogApi serverdata = flip runReaderT serverdata . unBlogApi
