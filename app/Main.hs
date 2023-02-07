module Main (main) where


import           Text.Read                    (readMaybe)
import           Database.Persist.Postgresql
import           Network.Wai.Handler.Warp     (run)
import           API
import           System.Environment           
import           Control.Concurrent           (threadDelay)
import qualified Data.ByteString.Char8 as C
import           Data.List.Split              (splitOn)
import           Control.Monad.Morph          (lift)




main :: IO ()
main = do
  print "Hello" 
  mbConnStr <- lookupEnv "db_connection_string"
  connStr <- case mbConnStr of 
               Just connStr -> return connStr
               Nothing -> error "Could not find 'db_connection_string' enviroment variable."
  mbPort <- lookupEnv "server_port"
  port <- case mbPort >>= (\s -> readMaybe s :: Maybe Int) of
            Just port -> return port
            Nothing -> error "Could not find or parse 'server_port' enviroment variable."
            
  run port $ app $ C.pack connStr