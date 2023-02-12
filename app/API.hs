module API where


import Database.Persist.Postgresql 
import Data.ByteString as BS
import Data.Text
import Data.Time.Calendar
import Servant
import Servant.Multipart
import Handlers.Handlers
import DBTypes
import CustomContentTypes




type ServerAPI =


        "users" 
            :> Header "limit" Int
            :> Header "offset-key" Text
            :> Get '[JSON] [UserProxyType]

   :<|> "users" 
            :> Capture "login" Text 
            :> Get '[JSON] UserProxyType  

   -- registration for all
   :<|> "users" 
            :> ReqBody '[JSON] UserProxyType 
            :> Post '[JSON] Bool  
   -- author / admin priveleges can grant only admin
   :<|> "users" :> "grant_author"
            :> Header "Authorization" Text 
			:> Capture "login" Text
			:> Patch '[JSON] Bool

   :<|> "users" :> "grant_admin"
            :> Header "Authorization" Text 
			:> Capture "login" Text
			:> Patch '[JSON] Bool


   :<|> "news" 
            -- maybe its possible to put in here some type which will assure header has correct format of (user:password)
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
            :> Header "Authorization" Text 
            :> MultipartForm Mem (MultipartData Mem) 
            :> Post '[JSON] Bool

   :<|>  "images" 
            :> Capture "image_id" Int
            :> Get '[JPEG] ByteString

   :<|>  "images_by_newsID" 
            :> Capture "news_id" Int
            :> Get '[JSON] [Image]


   :<|>  "category" 
            :> Capture "category_id" Text
            :> Get '[JSON] Category

   :<|>  "category" 
            :> Header "Authorization" Text 
            :> ReqBody '[JSON] Category 
            :> Post '[JSON] Bool


   -- audio stream source
   :<|> "audio" :> "download"
            :> Capture "audio_id" Int 
			:> StreamGet NoFraming OGG (Headers '[Header "content-disposition" Text] (SourceIO ByteString))

   :<|> "audio" :> "play"
            :> Header "range" Text
            :> Capture "audio_id" Int 
			:> GetPartialContent '[OctetStream] 
			  (Headers 
			  '[ Header "accept-ranges" Text
			   , Header "content-length" Int
			   , Header "content-range" Text
			   , Header "content-type" Text
			   ] 
			   ByteString )

   -- we can manage small audio in single piece
   :<|> "audio" :> "upload" :> "small"
            :> Header "Authorization" Text 
            :> Capture "audio_id" Int 
            :> MultipartForm Mem (MultipartData Mem) 
            :> Post '[JSON] Bool
   -- and big ones via stream
   :<|> "audio" :> "upload" :> "big"
            :> Header "Authorization" Text 
            :> Capture "audio_id" Int 
            :> StreamBody NoFraming OctetStream (SourceIO BS.ByteString)
            :> Post '[JSON] Bool



server :: ConnectionString -> Server ServerAPI
server connStr  = 
  (getUsersHandler connStr)        :<|> 
  (getSingleUserHandler connStr)   :<|>
  (createUserHandler connStr)      :<|>
  (grantAuthorHandler connStr)     :<|>
  (grantAdminHandler connStr)      :<|>
  
  (createNewsHandler connStr)      :<|>
  (getNewsHandler connStr)         :<|>  
  
  (postPhotosHandler connStr)      :<|> 
  (getImagesHandler connStr)       :<|> 
  (getImagesByNewsHandler connStr) :<|>
  
  (getCategoryHandler connStr)     :<|>
  (createCategoryHandler connStr)  :<|>
  
  (getAudioStreamHandler connStr)  :<|>
  (playAudioHandler connStr)       :<|>
  (uploadAudioHandler connStr)     :<|>
  (uploadAudioStreamHandler connStr)




userAPI :: Proxy ServerAPI
userAPI = Proxy



app :: ConnectionString -> Application
app connStr = serve userAPI ( server connStr )