module Handlers.Authorization where


import Control.Monad.IO.Class       (liftIO,MonadIO)
import System.Random                ( randomIO )

import Data.ByteString              ( ByteString, singleton )
import Data.Text                    ( Text, splitOn )
import Data.Text.Encoding           ( encodeUtf8 )
import Data.Word                    ( Word8 )

import Servant
import Crypto.KDF.PBKDF2 as PBKDF2
import Database.Persist.Postgresql 
import Data.Aeson                   ( encode )
import DBTypes
import Handlers.Primitives
  
  
saltingParameters = PBKDF2.Parameters { iterCounts = 10, outputLength = 20 }


credentialsError :: Handler a
credentialsError = throwError err401 { errBody = Data.Aeson.encode ("Wrong credentials." :: Text ) }



checkCredentials :: ConnectionString -> UserAuthority -> Maybe Text -> Handler ()
checkCredentials connStr authority credentials = do
  (login,pass) <- getCredentials credentials
  mbUser <- runDB connStr $ getSingleUser login
  user <- case mbUser of 
            [Entity _ user] -> return user
            _ -> credentialsError
  if user.userPassword == getHash (encodeUtf8 pass) user.userSalt
    then return ()
    else credentialsError
  case authority.check user of 
    True -> return ()
    False -> credentialsError
    
  where getSingleUser :: (MonadIO m) => Text -> SqlPersistT m [Entity User]
        getSingleUser login = selectList [UserLogin ==. login] [] 
  
  

getCredentials :: Maybe Text -> Handler (Text,Text)
getCredentials credentials = 
  case credentials of
    Just c -> 
      case splitOn ":" c of
        (login:pass:[]) -> return (login,pass)
        _ -> credentialsError
    Nothing -> credentialsError
    
    
    

getPasswordHash :: ByteString -> IO (ByteString,ByteString)
getPasswordHash password = do
  salt <- randomIO :: IO Word8
  let params = PBKDF2.Parameters { iterCounts = 10, outputLength = 20 }
      saltBS = singleton salt
  return (PBKDF2.fastPBKDF2_SHA512 params password saltBS,saltBS)



getHash :: ByteString -> ByteString -> ByteString
getHash pass salt = PBKDF2.fastPBKDF2_SHA512 saltingParameters pass salt
