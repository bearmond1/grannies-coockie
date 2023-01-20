{-# LANGUAGE TypeOperators #-}
module API where


import Database.Persist.Postgresql 
import Data.Text
import Servant
import Servant.Pagination
import Handlers
import DBTypes



type ServerAPI =
        "users" 
            :> Header "limit" Int
            :> Header "offset-key" Int
		    :> QueryParam "login" Text 
			-- :> Get '[JSON] [UserProxyType] 
			 :> Get '[JSON] [UserProxyType]
   :<|> "users" 
		    :> Capture "login" Text 
			:> Get '[JSON] UserProxyType  
   :<|> "users" 
		    :> ReqBody '[JSON] UserProxyType 
			:> Post '[JSON] Bool  
   :<|> "news" 
		    :> Header "Authorization" Text 
			:> ReqBody '[JSON] News 
			:> Post '[JSON] Bool  
   :<|>  "category" 
		    :> Header "Authorization" Text 
			:> ReqBody '[JSON] Category 
			:> Post '[JSON] Bool



server :: ConnectionString -> Server ServerAPI
server connStr  = 
  (getUsersHandler connStr) :<|> 
  (getSingleUserHandler connStr) :<|>
  (createUserHandler connStr) :<|>
  (createNewsHandler connStr) :<|>
  (createCategoryHandler connStr)  

userAPI :: Proxy ServerAPI
userAPI = Proxy


app :: ConnectionString -> Application
app connStr = serve userAPI ( server connStr )