module Handlers where


import Control.Monad.IO.Class       (liftIO,MonadIO)
import System.Random
import Data.Time.Calendar
import GHC.Generics ( Generic )
import Data.Maybe (fromMaybe)

import Database.Persist.Postgresql 
import Servant
import Servant.Pagination

import Data.Text hiding ( map, length )
import Data.Text.Encoding
import qualified Data.ByteString as BS
import Data.Aeson

import DBTypes
import Authorization




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
  
  

  
getUsersHandler :: ConnectionString -> Maybe Int -> Maybe Int -> Maybe Text -> Handler [UserProxyType]
getUsersHandler connStr mbLimit mboffset mbLogin = do
  liftIO $ print (mbLimit,mboffset)
  users <- runDB connStr getUsers mbLogin
  return $ fmap userToProxy users 



getUsers :: (MonadIO m) => Maybe Text -> SqlPersistT m [User]
getUsers mbLogin = do
  let filters = case mbLogin of 
                  Nothing -> []
                  (Just login) -> [UserLogin ==. login]
  list <- selectList filters []
  return $ map (\(Entity _ u) -> u) list




getSingleUserHandler :: ConnectionString -> Text -> Handler UserProxyType
getSingleUserHandler connStr login = do
  users <- runDB connStr getSingleUser login
  case users of 
    [Entity _ user] -> return $ userToProxy user
    _ -> credentialsError


getSingleUser :: (MonadIO m) => Text -> SqlPersistT m [Entity User]
getSingleUser login = selectList [UserLogin ==. login] [] 




createUserHandler :: ConnectionString -> UserProxyType -> Handler Bool
createUserHandler connStr user = runDB connStr writeNewUserDB (proxyToUser user) >> return True


writeNewUserDB :: (MonadIO m) => User -> SqlPersistT m User
writeNewUserDB user = do
  (pass_hash,salt) <- liftIO $ getPasswordHash user.userPassword
  insert user{ userPassword = pass_hash, userSalt = salt }
  return user
  
  
  
createNewsHandler :: ConnectionString -> Maybe Text -> News -> Handler Bool
createNewsHandler connStr credentials news = do
  checkCredentials connStr authorAuthority credentials
  id <- liftIO randomIO 
  let news' = news{ newsNews_id = id }
  runDB connStr insert news'
  return True




createCategoryHandler :: ConnectionString -> Maybe Text -> Category -> Handler Bool
createCategoryHandler connStr credentials category = do
  checkCredentials connStr adminAuthority credentials
  runDB connStr insert category
  return True