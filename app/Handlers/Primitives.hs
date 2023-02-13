{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module Handlers.Primitives where


import Database.Persist.Postgresql 
import Control.Monad.Reader         (ReaderT, runReaderT, lift)
import Control.Monad.Logger         (runStderrLoggingT,runStdoutLoggingT,LoggingT)
import Control.Monad.IO.Class       (liftIO,MonadIO)
import System.Log.FastLogger
import Servant



type HandlerLog = LogStr -> Handler ()

data SelectParams a = SelectParams
  { filters  :: [Filter a]
  , options  :: [SelectOpt a]
  }
  
  
  
runDB :: ConnectionString -> ( ReaderT SqlBackend (LoggingT IO ) b) -> Handler b
runDB connStr dbAction  = liftIO $ runStdoutLoggingT $ withPostgresqlConn connStr $ 
    \backend -> runReaderT (dbAction ) backend
	
	
fromEntities :: [Entity  b] -> [b]
fromEntities = Prelude.map (\(Entity _ u) -> u) 