module Main (main) where

import           Control.Monad.IO.Class       (liftIO,MonadIO)
import           Text.Read                    (readMaybe)
import           Data.Text as Text
import           Data.ByteString
import           Database.Persist.Postgresql 
import           Network.Wai.Handler.Warp     (run,runSettings,setLogger,setPort,defaultSettings)
import           API                          (app)
import           System.Environment        
import qualified Data.ByteString.Char8 as BS8
import           System.Log.FastLogger
import           Servant



-- expecting 'db_connection_string', 'server_port', 'ServerLogSpec' enviroment variables
main :: IO ()
main = do
  print "Hello" 
  
  connStr <- getConnStr
  port <- getPort  
  logger <- getLogger

  runApp port connStr logger


runApp :: Int -> ConnectionString -> (LogStr -> Handler ()) -> IO ()
runApp port connStr logger = run port $ app connStr logger




getLogger :: IO (LogStr -> Handler ())
getLogger = do
  mbLogSpec <- lookupEnv "ServerLogSpec"
  textSpec <- case mbLogSpec of
            Just spec -> return $ Text.pack spec
            Nothing -> error "Could not find 'ServerLogSpec' enviroment variable."
  
  (logFilePath,logFileSize',maxLogFiles',bufSize') <- case Text.splitOn ";" textSpec of
      (logFilePath:logFileSize:maxLogFiles:bufSize:[]) -> return (logFilePath,logFileSize,maxLogFiles,bufSize)
      smth -> error $ "Could not parse 'ServerLogSpec' enviroment variable." <> (Text.unpack $ Text.concat smth)

  logFileSize <- parseInt logFileSize' $ "Could not read '" <> logFileSize' <> "' as int."
  
  maxLogFiles <- parseInt maxLogFiles' $ "Could not read '" <> maxLogFiles' <> "' as int."
  
  bufSize <- parseInt bufSize' $ "Could not read '" <> bufSize' <> "' as int."
  
  (logger,_) <- newFastLogger $ LogFile ( FileLogSpec (Text.unpack logFilePath) (toInteger logFileSize) maxLogFiles) bufSize
  return $ liftIO . logger




parseInt :: Text -> Text -> IO Int
parseInt mbInt errorText = 
  case readMaybe $ Text.unpack mbInt of
    Just int -> return int
    Nothing -> error $ Text.unpack errorText




getConnStr :: IO ByteString
getConnStr = do  
  mbConnStr <- lookupEnv "db_connection_string"
  case mbConnStr of 
    Just connStr -> return $ BS8.pack connStr
    Nothing -> error "Could not find 'db_connection_string' enviroment variable."


getPort :: IO Int
getPort = do
  mbPort <- lookupEnv "server_port"
  case mbPort >>= (\s -> readMaybe s :: Maybe Int) of
    Just port -> return port
    Nothing -> error "Could not find or parse 'server_port' enviroment variable."