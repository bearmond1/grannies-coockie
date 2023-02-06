module API where


import Control.Monad.IO.Class       (liftIO,MonadIO)
import Database.Persist.Postgresql 
import Data.ByteString as BS
import Data.Text
import Data.Time.Calendar
import Servant
import Servant.Multipart
import Handlers.Handlers
import DBTypes



type ServerAPI =

        "users" 
            :> Header "limit" Int
            :> Header "offset-key" Text
            :> Get '[JSON] [UserProxyType]
   :<|> "users" 
            :> Capture "login" Text 
            :> Get '[JSON] UserProxyType  
   :<|> "users" 
            :> ReqBody '[JSON] UserProxyType 
            :> Post '[JSON] Bool  

   :<|> "news" 
            -- maybe its possible to put in here some type which will assure header has correct format
            :> Header "Authorization" Text 
            :> ReqBody '[JSON] News 
            :> Post '[JSON] Bool              
   :<|> "news" 
            :> Header "limit" Int
            :> Header "offset-key" Text
            -- it would be nice to write custom combinator which will get filtering fields from data structure
            :> QueryParam "createdAt" Day  
            :> QueryParam "createdBefore" Day  
            :> QueryParam "createdAfter" Day  
            :> QueryParams "author" Text
            :> QueryParams "category" Text
  
            :> QueryParam "sort_by" Text 
            :> Get '[JSON] [NewsProxyType]  

   :<|>  "images" 
            :> MultipartForm Mem (MultipartData Mem) 
            :> Post '[JSON] Bool

   :<|>  "images" 
            :> Capture "image_id" Int
            :> Get '[JSON] Image
			
   :<|>  "images_by_newsID" 
            :> Capture "news_id" Int
            :> Get '[JSON] [Image]


   :<|>  "category" 
            :> Header "Authorization" Text 
            :> ReqBody '[JSON] Category 
            :> Post '[JSON] Bool

   :<|> "audio"
            :> Capture "audio_id" Int 
			:> StreamGet NoFraming OctetStream (SourceIO ByteString)

   :<|> "upload_audio"
            :> Post '[JSON] Bool



server :: ConnectionString -> Server ServerAPI
server connStr  = 
  (getUsersHandler connStr)        :<|> 
  (getSingleUserHandler connStr)   :<|>
  (createUserHandler connStr)      :<|>
  
  (createNewsHandler connStr)      :<|>
  (getNewsHandler connStr)         :<|>  
  
  (postPhotosHandler connStr)      :<|> 
  (getImagesHandler connStr)       :<|> 
  (getImagesByNewsHandler connStr) :<|>
  
  (createCategoryHandler connStr)  :<|>
  
  (getAudioStreamHandler connStr)  :<|>
  (uploadFileToDb connStr)




userAPI :: Proxy ServerAPI
userAPI = Proxy


app :: ConnectionString -> Application
app connStr = serve userAPI ( server connStr )