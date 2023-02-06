module Handlers.AudioHandlers where


import           Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           Database.Persist.Postgresql 
import           Data.Text as Text
import           Control.Concurrent           (threadDelay)
import           Control.Monad                (liftM, forM_)
import           Control.Monad.Morph          ( lift )
import           Control.Monad.IO.Class       (liftIO,MonadIO)
import           Control.Monad.Reader         (ReaderT, runReaderT, lift)
import           Control.Monad.Logger         (runStderrLoggingT,runStdoutLoggingT,LoggingT)
import           Servant.Types.SourceT
import           Servant
import           Servant.API.Stream
import           System.Random
import           Handlers.Primitives
import           DBTypes


type AudioID = Int
type AudioPieceIndex = Int


getAudioStreamHandler :: ConnectionString -> AudioID -> Handler ( SourceIO ByteString )
getAudioStreamHandler connStr audioID = (liftIO $ getNextStep connStr audioID 0) >>= return . fromStepT 
  where         
    getNextStep :: ConnectionString -> AudioID -> AudioPieceIndex -> IO (StepT IO ByteString)
    getNextStep connStr audioID index = do 
       audioPiece <- runStdoutLoggingT $ withPostgresqlConn connStr $ 
                   \backend -> runReaderT ( getAuidioPieceDB  audioID index ) backend
       if audioPiece == BS.empty 
         then return Stop
         else return $ Yield audioPiece (Effect $ getNextStep connStr audioID $ index + 1)



getAuidioPieceDB :: (MonadIO m) => AudioID -> AudioPieceIndex -> SqlPersistT m ByteString
getAuidioPieceDB audioID index = do
  result <- selectList [AudioAudio_id ==. audioID, AudioIndex ==. index] [] 
  case result of
    [Entity _ (Audio{ audioContent })] -> return audioContent
    _ -> return BS.empty
    
    
    
uploadFileToDb :: ConnectionString -> Handler Bool
uploadFileToDb connStr = do
  file <- liftIO $ BS.readFile "C:\\Users\\madOr\\Downloads\\npp.8.4.8.Installer.x64.exe"
  id <- liftIO randomIO :: Handler Int
  -- split file into 1Mb chunks
  -- check for more efficient way
  let (byteStrList,_,_) = myFunc ([], file, 0)
      megabyte = 2^20 :: Int
      myFunc :: ([(Int,ByteString)],ByteString,Int) -> ([(Int,ByteString)],ByteString,Int)
      myFunc (list, file, index) =  
           let (chunk,rest) = BS.splitAt megabyte file
           in if BS.length file > megabyte
                 then myFunc ((list <> [(index,chunk)]) , rest, index + 1)
                 else (list <> [(index,chunk)],BS.empty,index)
      audioArray = Prelude.map (\(index,bytestr) -> Audio{ audioAudio_id = id, audioIndex = index, audioContent = bytestr } ) byteStrList
  mapM_ (\audio -> runDB connStr $ insert audio) audioArray
  return True