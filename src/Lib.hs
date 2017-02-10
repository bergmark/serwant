{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib
  ( startApp
  ) where

import MyPrelude

import Control.Monad.Except
import qualified Data.Set as Set
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import ApiTypes
import Type.Range (Range (Range))
import Type.ToServantErr (ToServantErr(..))
import Type.User (User (User))
import Type.UserInfo (UserInfo)
import Type.UserPost (UserPost)
import qualified Api.Post
import qualified Api.User
import qualified Type.Post
import qualified Type.Post as Type (Post)

type Api
     = "user" :> UserApi
  :<|> "post" :> PostApi

type UserApi
     = ReqBody '[JSON] User :> Post '[JSON] UserInfo
  :<|> Ranged (Get '[JSON] [UserInfo])

type PostApi
     = ReqBody '[JSON] UserPost :> Post '[JSON] Type.Post
  :<|> Ranged (Get '[JSON] [Type.Post])
  :<|> PostByIdentifier (Get '[JSON] Type.Post)
  :<|> PostByIdentifier (Delete '[JSON] ())

type PostByIdentifier x
     = "id" :> Capture "postId" Word :> x
  :<|> "latest" :> x

type Ranged a
  = QueryParam "offset" Word
 :> QueryParam "limit" Word
 :> a

api :: Proxy Api
api = Proxy

blogApiToHandler' :: ServerData -> (BlogApi :~> Handler)
blogApiToHandler' sd = NT $ runBlogApi sd

app :: ServerData -> Application
app sd = serve api (server sd)

-- server :: ServerData -> ServerT Api (ExceptT ServantErr IO)
server :: ServerData -> Server Api
server sd = enter (blogApiToHandler' sd) server'

server' :: ServerT Api BlogApi
server' = userServer :<|> postServer
  where
    userServer
         = jsonErr . Api.User.create
      :<|> Api.User.list .: Range
    postServer
         = jsonErr . Api.Post.create
      :<|> Api.Post.list .: Range
      :<|> postByIdentifier Api.Post.get
      :<|> postByIdentifier Api.Post.remove

postByIdentifier
  :: (ToServantErr a, ToJSON a)
  => (Api.Post.Identifier -> ExceptT a BlogApi b)
  -> (Word -> BlogApi b) :<|> BlogApi b
postByIdentifier f
     = jsonErr . f . Api.Post.ById
  :<|> jsonErr (f Api.Post.Latest)

jsonErr :: (ToServantErr a, ToJSON a) => ExceptT a BlogApi b -> BlogApi b
jsonErr mb =
  runExceptT mb >>= \case
    Left err -> throwError (toServantErr err) {errBody = encode err}
    Right res -> pure res

startApp :: IO ()
startApp = do
  sd <- exampleBlog
  run 8080 (app sd)

exampleBlog :: IO ServerData
exampleBlog = ServerData <$> newTVarIO mockUsers <*> newTVarIO mockPosts
--        <*> newTVarIO mockComments

-- | Prepoulated users
mockUsers :: Set User
mockUsers = Set.fromList
  [ User "adam" "1234"
  , User "erik" "2345"
  , User "sebas" "3456"
  ]

-- | Prepopulated posts
mockPosts :: Set Type.Post.Post
mockPosts = Set.fromList
  [ Type.Post.Post 0 "adam" (readNote "mockPost date" "2014-03-31 15:34:00") "First post" "Hello world!"
  , Type.Post.Post 1 "erik" (readNote "mockPost date" "2014-04-01 13:37:00") "Rest is awesome" "Just wanted to tell the world!"
  ]

-- mockComments :: HashMap Int (Set Comment)
-- mockComments = H.fromList
--   [(0, Set.fromList [Comment "adam" (read "2014-06-08 14:00:00") "This is the best post I've ever written, please be gentle"])]
