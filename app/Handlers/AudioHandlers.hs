module Handlers.AudioHandlers where


import           Data.Aeson as Aeson
import           Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           Database.Persist.Postgresql 
import           Data.Text as Text
import           Text.Read                    (readMaybe)
import           Data.String                  (fromString)
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


-- getAudioStreamHandler :: ConnectionString -> AudioID -> Handler ( SourceIO ByteString )
getAudioStreamHandler connStr audioID = 
  let header = "attachment; filename=\"" <> fromString (show audioID) <> ".ogg\"" :: Text
  in (liftIO $ getNextStep connStr audioID 0) >>= return . addHeader header . fromStepT 
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

  
  
  
  
uploadAudioHandler :: ConnectionString -> Maybe Text -> AudioID ->  MultipartData Mem -> Handler Bool
uploadAudioHandler connStr credentials audioID multipartData = 
  let [contents] = Prelude.map (LBS.toStrict . fdPayload) $ files multipartData
  in writeAudiotoDB connStr credentials audioID contents
 
 
    
uploadAudioStreamHandler :: ConnectionString -> Maybe Text -> AudioID -> SourceIO ByteString -> Handler Bool
uploadAudioStreamHandler connStr credentials audioID source = do
  audio <- liftIO $ runExceptT $ runSourceT source
  case audio of 
    Right list -> writeAudiotoDB connStr credentials audioID $ BS.concat list
    _ -> throwError err400
  
  
writeAudiotoDB :: ConnectionString -> Maybe Text -> AudioID -> ByteString -> Handler Bool
writeAudiotoDB connStr credentials audioID content = do
  checkCredentials connStr authorAuthority credentials
  let (byteStrList,_,_) = to1MbChunks ([], content, 0)
      bytestrToObject = \(index,bytestr) -> Audio{ audioAudio_id = audioID, audioIndex = index, audioContent = bytestr }
      audioArray = Prelude.map bytestrToObject byteStrList
  mapM_ (\audio -> runDB connStr $ insert audio) audioArray
  return True
  
  
to1MbChunks :: ([(Int,ByteString)],ByteString,Int) -> ([(Int,ByteString)],ByteString,Int)
to1MbChunks (list, file, index) =  
  let (chunk,rest) = BS.splitAt megabyte file
  in if BS.length file > megabyte
      then to1MbChunks ((list <> [(index,chunk)]) , rest, index + 1)
      else (list <> [(index,chunk)],BS.empty,index)


megabyte = 2^20 :: Int




-- playAudioHandler :: ConnectionString -> Int -> Handler ByteString
playAudioHandler connStr mbRange audioID = do
  liftIO $ print "handler"
  header <- case mbRange of
              Just range -> return range
              _ -> return "bytes=0-"
  liftIO $ print header
  let dropSize = Text.takeWhile (/= '/') header
  range <- case splitOn "=" dropSize of
             (_:range:[]) -> return range
             x -> throwError err400 { errBody = Aeson.encode x }
  liftIO $ print range
  (from',to') <- case splitOn "-" range of
                 (from:to:[]) -> return ( Text.unpack from, Text.unpack to)
                 _ -> throwError err400
  liftIO $ print (from',to')
  (from,to) <- case (readMaybe from' :: Maybe Int, readMaybe to' :: Maybe Int) of
                  (Just from,Just to) -> return (from,to)
                  (Just from, Nothing) -> return (from,2^20)
                  (a,b) -> (liftIO $ print (from',to')) >> throwError err400 { errBody = Aeson.encode (a,b) }
  liftIO $ print (from,to)
  let fromIndex = div from megabyte
      skip = mod from megabyte
      left = mod to megabyte
      toIndex = if left == 0
                  then div to megabyte
                  else (div to megabyte) + 1
  byteStrings <- runDB connStr $ selectList [AudioAudio_id ==. audioID, AudioIndex >=. fromIndex, AudioIndex <=. toIndex] []
  let byteStr = BS.concat $ Prelude.map (\Audio{ audioContent } -> audioContent) $ fromEntities byteStrings
      result = BS.take ((BS.length byteStr) - skip - left) $ BS.drop skip byteStr

      accept_ranges req = addHeader "bytes" req
      content_length req = addHeader (BS.length result) req
      content_range req = addHeader ( "bytes " <> Text.pack (show from) <> "-" <> Text.pack (show to) <> "/*" :: Text) req
      content_type req = addHeader "application/ogg" req
      
  return . accept_ranges . content_length . content_range . content_type $ result  