module Handlers.AudioHandlers where


import           Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           Database.Persist.Postgresql 
import           Data.Text as Text
import           Control.Monad.IO.Class       (liftIO,MonadIO)
import           Control.Monad.Reader         (ReaderT, runReaderT, lift)
import           Control.Monad.Logger         (runStderrLoggingT,runStdoutLoggingT,LoggingT)
import           Control.Monad.Except         (runExceptT)
import           Servant.Types.SourceT
import           Servant
import           Servant.Multipart
import           System.Random
import           Handlers.Primitives
import           Handlers.Authorization
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

  
  
  
  
uploadAudioHandler :: ConnectionString -> Maybe Text -> MultipartData Mem -> Handler Bool
uploadAudioHandler connStr credentials multipartData = do
  checkCredentials connStr authorAuthority credentials
  -- we expect one file in request
  let [contents] = Prelude.map (LBS.toStrict . fdPayload) $ files multipartData
  id <- liftIO randomIO :: Handler Int
  let (byteStrList,_,_) = to1MbChunks ([], contents, 0)
      bytestrToObject = \(index,bytestr) -> Audio{ audioAudio_id = id, audioIndex = index, audioContent = bytestr }
      audioArray = Prelude.map bytestrToObject byteStrList
  mapM_ (\audio -> runDB connStr $ insert audio) audioArray
  return True
  

to1MbChunks :: ([(Int,ByteString)],ByteString,Int) -> ([(Int,ByteString)],ByteString,Int)
to1MbChunks (list, file, index) =  
  let (chunk,rest) = BS.splitAt megabyte file
  in if BS.length file > megabyte
      then to1MbChunks ((list <> [(index,chunk)]) , rest, index + 1)
      else (list <> [(index,chunk)],BS.empty,index)
  where 
    megabyte = 2^20 :: Int
	
	
	
	
uploadAudioStreamHandler :: ConnectionString -> Maybe Text -> AudioID -> SourceIO ByteString -> Handler Bool
uploadAudioStreamHandler connStr credentials audioID source = do
  checkCredentials connStr authorAuthority credentials
  audioPiece <- liftIO $ runExceptT $ runSourceT source
  liftIO $ print audioPiece
 -- runDB connStr $ insert 
  return True