module Handlers.UserHandlers where



import Control.Monad.IO.Class       (liftIO,MonadIO)
import Data.Time.Calendar
import GHC.Generics                 ( Generic )

import Database.Persist.Postgresql 
import Database.Persist.Types
import Servant

import Data.Text hiding             ( map, length )
import Data.Text.Encoding
import Data.Aeson

import DBTypes
import Handlers.Authorization
import Handlers.Primitives
import System.Log.FastLogger






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
    deriving (Eq,Show,Generic,ToJSON,FromJSON)
    
    
userToProxy :: User -> UserProxyType
userToProxy (User {..}) =
  UserProxyType { pass = "", login = userLogin, email = userEmail, phone_number = userPhone_number, join_date = userJoin_date, admin = userAdmin, author = userAuthor }


proxyToUser :: UserProxyType -> User
proxyToUser (UserProxyType{..}) = User login (encodeUtf8 pass) "" email phone_number join_date admin author
  
  

  
getUsersHandler :: ConnectionString -> HandlerLog -> Maybe Int -> Maybe Text -> Handler [UserProxyType]
getUsersHandler connStr logger mbLimit mbOffsetKey = do
  let queryParams = getFetchUsersParams mbLimit mbOffsetKey
  users <- runDB connStr $ getUsers queryParams
  logger $ "Fetched users: " <> (toLogStr $ show users)
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
getUsers SelectParams{..} = fmap fromEntities $ selectList filters options



getSingleUserHandler :: ConnectionString -> HandlerLog -> Text -> Handler UserProxyType
getSingleUserHandler connStr logger login = do
  users <- runDB connStr $ getSingleUser login
  case users of 
    [Entity _ user] -> do
      logger $ "Fetched user: " <> (toLogStr $ show users)
      return $ userToProxy user
    _ -> throwError err400


getSingleUser :: (MonadIO m) => Text -> SqlPersistT m [Entity User]
getSingleUser login = selectList [UserLogin ==. login] [] 




createUserHandler :: ConnectionString -> HandlerLog -> UserProxyType -> Handler Bool
createUserHandler connStr logger user = do
  runDB connStr $ writeNewUserDB (proxyToUser user) 
  logger $ "Created user: " <> (toLogStr $ show user)
  return True


writeNewUserDB :: (MonadIO m) => User -> SqlPersistT m User
writeNewUserDB user = do
  (pass_hash,salt) <- liftIO $ getPasswordHash user.userPassword
  insert user{ userPassword = pass_hash, userSalt = salt, userAdmin = False, userAuthor = False }
  return user
  
  
  
grantAuthorHandler :: ConnectionString -> HandlerLog -> Maybe Text -> Text -> Handler Bool
grantAuthorHandler connStr logger credentials login = do
  admin <- checkCredentials connStr adminAuthority credentials
  let psVal = PersistText login
  eithrKey <- case keyFromValues [psVal] of
                Left text -> credentialsError
                Right key -> return key
  runDB connStr $ update eithrKey [UserAuthor =. True]
  logger $ 
    "Granted author privileges to user " <> (toLogStr login) <>
	" by " <> (toLogStr admin) <> "."
  return True
  
  
  

grantAdminHandler :: ConnectionString -> HandlerLog -> Maybe Text -> Text -> Handler Bool
grantAdminHandler connStr logger credentials login = do
  admin <- checkCredentials connStr adminAuthority credentials
  let psVal = PersistText login
  eithrKey <- case keyFromValues [psVal] of
                     Left text -> credentialsError
                     Right key -> return key
  runDB connStr $ update eithrKey [UserAdmin =. True]
  logger $ 
    "Granted admin privileges to user " <> (toLogStr login) <>
	" by " <> (toLogStr admin) <> "."
  return True
  