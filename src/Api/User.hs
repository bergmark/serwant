module Api.User where

import MyPrelude

import qualified Data.Foldable as F
import qualified Data.Set      as Set
import qualified Data.Text     as T

import ApiTypes (BlogApi, ServerData (..))
import Type.User (User)
import Type.UserInfo (UserInfo (..))
import Type.Range (Range)
import qualified Type.Range as Range
import Type.UserSignupError (UserSignupError (..))
import qualified Type.User as User
import qualified Type.UserInfo as UserInfo

list :: Range -> BlogApi [UserInfo]
list r = do
  usrs <- liftIO . atomically . readTVar =<< asks users
  pure . fmap toUserInfo . Range.list r . Set.toList $ usrs

create :: User -> ExceptT UserSignupError BlogApi UserInfo
create usr = do
  usrs <- asks users
  merr <- liftIO . atomically $ do
    vu <- validUserName usr <$> readTVar usrs
    if not (validPassword usr)
      then pure . Just $ InvalidPassword
      else if not vu
        then pure . Just $ InvalidUserName
        else modifyTVar usrs (Set.insert usr) *> pure Nothing
  maybe (pure $ toUserInfo usr) throwError merr

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
