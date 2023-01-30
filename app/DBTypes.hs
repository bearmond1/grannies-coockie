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
import           Data.Int
import           Data.ByteString
import           GHC.Generics (Generic)
import           Data.Proxy (Proxy (..))


-- to remove prefix and implicit ID column
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
    newsId Int
    Primary newsId 
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
	
Image 
    news_id Int
	image_id Int
	Primary news_id image_id
	content ByteString
	deriving Show
	
|]


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