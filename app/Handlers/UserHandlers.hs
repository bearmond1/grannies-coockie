module Handlers.UserHandlers where



import Control.Monad.IO.Class       (liftIO,MonadIO)
import System.Random
import Data.Time.Calendar
import GHC.Generics ( Generic )

import Database.Persist.Postgresql 
import Servant

import Data.Text hiding ( map, length )
import Data.Text.Encoding
import Data.Aeson

import DBTypes
import Handlers.NewsHandlers
import Handlers.Authorization
import Handlers.Primitives






data UserProxyType 
  = UserProxyType
    { login :: Text
    , pass :: Text
    , email :: Text
    , phone_number :: Text
    , join_date :: Day
    , admin :: Bool
    , author :: Bool
    }
    deriving (Eq,Show, Generic, ToJSON, FromJSON)
    
    
userToProxy :: User -> UserProxyType
userToProxy (User {..}) =
  UserProxyType { pass = "", login = userLogin, email = userEmail, phone_number = userPhone_number, join_date = userJoin_date, admin = userAdmin, author = userAuthor }


proxyToUser :: UserProxyType -> User
proxyToUser (UserProxyType{..}) = User login (encodeUtf8 pass) "" email phone_number join_date admin author
  
  

  
getUsersHandler :: ConnectionString -> Maybe Int -> Maybe Text -> Handler [UserProxyType]
getUsersHandler connStr mbLimit mbOffsetKey = do
  let queryParams = getFetchUsersParams mbLimit mbOffsetKey
  users <- runDB connStr $ getUsers queryParams
  return $ fmap userToProxy users 



getFetchUsersParams :: Maybe Int -> Maybe Text -> SelectParams User
getFetchUsersParams mbLimit mbOffsetKey =
  let filters = case mbOffsetKey of 
                 Just k -> [UserLogin >. k]
                 Nothing -> []
      options = case mbLimit of
                 Just l -> [LimitTo l,Asc UserLogin]
                 Nothing -> [Asc UserLogin]
  in SelectParams {..}


getUsers :: (MonadIO m) => SelectParams User -> SqlPersistT m [User]
getUsers SelectParams{..} = do
  list <- selectList filters options
  return $ map (\(Entity _ u) -> u) list




getSingleUserHandler :: ConnectionString -> Text -> Handler UserProxyType
getSingleUserHandler connStr login = do
  users <- runDB connStr $ getSingleUser login
  case users of 
    [Entity _ user] -> return $ userToProxy user
    _ -> credentialsError


getSingleUser :: (MonadIO m) => Text -> SqlPersistT m [Entity User]
getSingleUser login = selectList [UserLogin ==. login] [] 




createUserHandler :: ConnectionString -> UserProxyType -> Handler Bool
createUserHandler connStr user = runDB connStr $ writeNewUserDB (proxyToUser user) >> return True


writeNewUserDB :: (MonadIO m) => User -> SqlPersistT m User
writeNewUserDB user = do
  (pass_hash,salt) <- liftIO $ getPasswordHash user.userPassword
  insert user{ userPassword = pass_hash, userSalt = salt }
  return user