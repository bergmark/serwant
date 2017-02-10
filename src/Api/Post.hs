module Api.Post
  ( Identifier (..)
  , create
  , list
  , get
  , remove
  ) where

import MyPrelude

import Data.Set (Set)
import qualified Data.Foldable as F
import qualified Data.Set      as Set
import qualified Data.Text     as T

import ApiTypes
import Type.CreatePost (CreatePost)
import qualified Type.Range as Range
import Type.Range (Range)
import Type.Post (Post (Post))
import Type.PostError (PostError (..))
import Type.Reason
import Type.User (User)
import Type.UserPost (UserPost (UserPost))
import qualified Type.CreatePost as CreatePost
import qualified Type.Post       as Post
import qualified Type.User       as User

data Identifier
  = Latest
  | ById Word
  deriving (Eq, Show)

-- | Defines the /post api end-point.
-- resource :: Resource BlogApi WithPost Identifier () Void
-- resource = mkResourceReader
--   { R.name   = "post" -- Name of the HTTP path segment.
--   , R.schema = withListing () $ named [("id", singleRead ById), ("latest", single Latest)]
--   , R.list   = const list -- list is requested by GET /post which gives a listing of posts.
--   , R.create = Just create -- PUT /post to create a new Post.
--   , R.get    = Just get
--   , R.remove = Just remove
--   }

postFromIdentifier :: Identifier -> TVar (Set Post) -> STM (Maybe Post)
postFromIdentifier i pv = finder <$> readTVar pv
  where
    finder = case i of
      ById ident -> find ((== ident) . Post.id) . Set.toList
      Latest     -> headMay . sortBy (flip $ comparing Post.createdTime) . Set.toList

get :: Identifier -> ExceptT (Reason Void) BlogApi Post
get i = do
  mpost <- atomicallyIO . postFromIdentifier i =<< asks posts
  case mpost of
    Nothing -> throwError NotFound
    Just a  -> pure a

-- | List Posts with the most recent posts first.
list :: Range -> BlogApi [Post]
list r = do
  psts <- atomicallyIO . readTVar =<< asks posts
  pure . Range.list r . sortBy (flip $ comparing Post.createdTime) . Set.toList $ psts

create :: UserPost -> ExceptT PostError BlogApi Post
create (UserPost usr pst) = do
  -- Make sure the credentials are valid
--  checkLogin usr
  pstsVar <- asks posts
  psts <- atomicallyIO . readTVar $ pstsVar
  post <- liftIO $ toPost (fromIntegral (Set.size psts) + 1) usr pst
  -- Validate and save the post in the same transaction.
  merr <- atomicallyIO $ do
    let vt = validTitle pst psts
    if not vt
      then pure . Just $ InvalidTitle
      else if not (validContent pst)
        then pure . Just $ InvalidContent
        else modifyTVar pstsVar (Set.insert post) *> pure Nothing
  maybe (pure post) throwError merr

remove :: Identifier -> ExceptT (Reason Void) BlogApi ()
remove i = do
  pstsVar <- asks posts
  merr <- atomicallyIO $ do
    mpost <- postFromIdentifier i pstsVar
    case mpost of
      Nothing -> pure . Just $ NotFound
      Just post -> modifyTVar pstsVar (Set.delete post) *> pure Nothing
  maybe (pure ()) throwError merr

-- | Convert a User and CreatePost into a Post that can be saved.
toPost :: Word -> User -> CreatePost -> IO Post
toPost i u p = do
  t <- getCurrentTime
  pure Post
    { Post.id          = i
    , Post.author      = User.name u
    , Post.createdTime = t
    , Post.title       = CreatePost.title p
    , Post.content     = CreatePost.content p
    }

-- | A Post's title must be unique and non-empty.
validTitle :: CreatePost -> Set Post -> Bool
validTitle p psts =
  let pt        = CreatePost.title p
      nonEmpty  = (>= 1) . T.length $ pt
      available = F.all ((pt /=) . Post.title) psts
  in available && nonEmpty

-- | A Post's content must be non-empty.
validContent :: CreatePost -> Bool
validContent = (>= 1) . T.length . CreatePost.content

-- | Throw an error if the user isn't logged in.
checkLogin :: User -> ExceptT (Reason e) BlogApi ()
checkLogin usr = do
  usrs <- liftIO . atomically . readTVar =<< asks users
  unless (usr `F.elem` usrs) $ throwError NotAllowed
