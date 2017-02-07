module Api.User where

import MyPrelude

import qualified Data.Foldable as F
import qualified Data.Set      as Set
import qualified Data.Text     as T

import ApiTypes (BlogApi, ServerData (..))
import Type.User (User)
import Type.UserInfo (UserInfo (..))
import Type.UserSignupError (UserSignupError (..))
import qualified Type.User as User
import qualified Type.UserInfo as UserInfo

-- | User extends the root of the API with a reader containing the ways to identify a user in our URLs.
-- Currently only by the user name.
-- type WithUser = ReaderT User.Name BlogApi

-- | Defines the /user api end-point.
-- resource :: Resource BlogApi WithUser User.Name () Void
-- resource = mkResourceReader
--   { R.name   = "user" -- Name of the HTTP path segment.
--   , R.schema = withListing () $ named [("name", singleBy T.pack)]
--   , R.list   = const list -- requested by GET /user, gives a paginated listing of users.
--   , R.create = Just create -- PUT /user creates a new user
--   }

list :: Maybe Word -> Maybe Word -> BlogApi [UserInfo]
list mOffset mLimit = do
  let offset = maybe id (drop . fromIntegral) mOffset
  let limit = maybe id (take . fromIntegral) mLimit
  usrs <- liftIO . atomically . readTVar =<< asks users
  pure . fmap toUserInfo . limit . offset . Set.toList $ usrs

create :: User -> ExceptT UserSignupError BlogApi UserInfo
create usr = do
  usrs <- asks users
  merr <- liftIO . atomically $ do
    vu <- validUserName usr <$> readTVar usrs
    if not (validPassword usr)
      then pure . Just $ InvalidPassword
      else if not vu
        then pure . Just $ InvalidUserName
        else modifyTVar usrs (Set.insert usr) >> pure Nothing
  maybe (pure $ toUserInfo usr) throwError merr

domainReason :: a -> UserSignupError
domainReason = error "domainReason"

-- | Convert a User into a representation that is safe to show to the public.
toUserInfo :: User -> UserInfo
toUserInfo u = UserInfo { UserInfo.name = User.name u }

validPassword :: User.User -> Bool
validPassword = (> 1) . T.length . User.password

validUserName :: User -> Set User -> Bool
validUserName u usrs =
  let un        = User.name u
      available = F.all ((un /=). User.name) usrs
      nonEmpty  = (> 1) . T.length $ un
  in available && nonEmpty
