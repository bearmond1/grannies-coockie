{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
module DBTypes where

import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Logger    (runStderrLoggingT)
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           Data.Time.Calendar
import           Data.Text
import           Data.ByteString
import           GHC.Generics (Generic)
import           Servant.Pagination
import           Data.Proxy (Proxy (..))



share [mkPersist sqlSettings] [persistLowerCase|
User 
    login Text
    Primary login
    password ByteString
	salt ByteString
    email Text
    phone_number Text
    join_date Day 
    admin Bool
    author Bool
    deriving Show
	
News json
    news_id Int
	Primary news_id 
	creation_date Day
	category Text
	header Text
	content Text
	is_published Bool
	author Text
    deriving Show
	
Category json
    name Text
	Primary name
	parent (Maybe Text)
	deriving Show
|]

--makeLenses ''User

instance Generic User 


data UserAuthority 
  = Admin { check :: User -> Bool }
  | Author { check :: User -> Bool }
  | NoAuthority { check :: User -> Bool }
  
adminAuthority :: UserAuthority
adminAuthority = Admin $ \user -> user.userAdmin

authorAuthority :: UserAuthority
authorAuthority = Author $ \user -> user.userAuthor

noAuthority :: UserAuthority
noAuthority = NoAuthority $ \_ -> True



instance HasPagination User "login" where
  type RangeType User "login" = Text
  getFieldValue _ = userLogin
  
defaultRange :: Range "login" Text
defaultRange = getDefaultRange (Proxy @User)  