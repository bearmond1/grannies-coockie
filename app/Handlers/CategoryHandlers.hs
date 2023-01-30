module Handlers.CategoryHandlers where

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