module Handlers.CategoryHandlers where


import Data.Aeson as Aeson
import Database.Persist.Postgresql 
import Data.Text hiding ( map, length )
import DBTypes
import Servant
import Handlers.Authorization
import Handlers.Primitives



createCategoryHandler :: ConnectionString -> Maybe Text -> Category -> Handler Bool
createCategoryHandler connStr credentials category = do
  checkCredentials connStr adminAuthority credentials
  runDB connStr $ insert category
  return True
  
  
getCategoryHandler :: ConnectionString -> Text -> Handler Category
getCategoryHandler connStr categoryName = do
  result <- runDB connStr $ selectList [CategoryName ==. categoryName] []
  case result of
    [Entity _ category] -> return category
    _ -> throwError err404 { errBody = Aeson.encode ("Not Found" :: Text) }