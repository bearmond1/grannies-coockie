module Handlers.CategoryHandlers where


import Data.Aeson as Aeson
import Database.Persist.Postgresql 
import Data.Text hiding ( map, length )
import DBTypes
import Servant
import Handlers.Authorization
import Handlers.Primitives
import System.Log.FastLogger



createCategoryHandler :: ConnectionString -> HandlerLog -> Maybe Text -> Category -> Handler Bool
createCategoryHandler connStr logger credentials category = do
  admin <- checkCredentials connStr adminAuthority credentials
  runDB connStr $ insert category
  logger . toLogStr $ "Created new category - '" <> category.categoryName <> "' by " <> admin
  return True
  
  
getCategoryHandler :: ConnectionString -> HandlerLog -> Text -> Handler Category
getCategoryHandler connStr logger categoryName = do
  logger . toLogStr $ "Fetching category " <> categoryName
  result <- runDB connStr $ selectList [CategoryName ==. categoryName] []
  case result of
    [Entity _ category] -> return category
    _ -> throwError err404 { errBody = Aeson.encode ("Not Found" :: Text) }