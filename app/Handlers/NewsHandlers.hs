module Handlers.NewsHandlers where
  
import Control.Monad  (liftM, forM_)
import Control.Monad.IO.Class       (liftIO,MonadIO)
import Control.Monad.Logger         (runStderrLoggingT,runStdoutLoggingT,LoggingT,MonadLogger)
import System.Random
import Data.Time.Clock
import Data.Time.Calendar
import GHC.Generics ( Generic )

import Database.Persist.Postgresql 
import Servant
import Servant.Multipart
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.MultipartFormData
-- import Database.Esqueleto.Experimental ( (^.), (?.), (:&) )
-- import qualified Database.Esqueleto.Experimental as E

import Data.Text  as Text hiding ( map, length )
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Text.Read ( readMaybe )
import Data.Text.Encoding
import Data.Aeson as Aeson

import DBTypes
import Handlers.Authorization
import Handlers.Primitives

import qualified Data.ByteString.Lazy as LBS





data NewsProxyType
  = NewsProxyType
  { newsId :: Int 
  , creation_date :: Day
  , category :: Text
  , header :: Text
  , content :: Text
  , is_published :: Bool
  , author :: Text
  , imagesURLs :: [Text]
  }
  deriving  (Eq, Show, Generic, ToJSON, FromJSON)

-- given list of news and their images we build   Map NewsID [Image]   for efficient processing 
-- and assemble list of API-suitable obejcts
getNewsProxy :: [News] -> [Image] -> [NewsProxyType]
getNewsProxy news images = Prelude.map getProxyFromNews news
  where tuplesList = Prelude.map (\image -> (imageNews_id image,image) ) images
        imagesTable = List.foldr ( \(imageID,image) hm -> Map.insertWith (<>) imageID [image] hm ) ( Map.empty :: Map.Map Int [Image] ) tuplesList
        getImages newsID = Map.findWithDefault [] newsID imagesTable
        getImagesURLs newsID = Prelude.map (\image -> "localhost:3000/images/" <> (pack . show $ imageImage_id image)) (getImages newsID)
        getProxyFromNews News{..} = 
          NewsProxyType { newsId = newsNewsId
                        , creation_date = newsCreation_date
                        , header = newsHeader
                        , content = newsContent
                        , is_published = newsIs_published
                        , author = newsAuthor
                        , category = newsCategory
                        , imagesURLs = getImagesURLs newsNewsId }
  

  
createNewsHandler :: ConnectionString -> Maybe Text -> News -> Handler Bool
createNewsHandler connStr credentials news = do
  checkCredentials connStr authorAuthority credentials
  id <- liftIO randomIO 
  let news' = news{ newsNewsId = id } 
  runDB connStr $ insert news'
  return True



-- type NewsQueryParams = Maybe Int -> Maybe Int -> Maybe Day -> Maybe Day -> Maybe Day -> [Text] -> [Text]
-- is it possible to do smth with this huge signature?
getNewsHandler :: ConnectionString -> 
  Maybe Int -> Maybe Text -> Maybe Day -> Maybe Day -> Maybe Day -> [Text] -> [Text] -> Maybe Text
  -> Handler [NewsProxyType] 
getNewsHandler connStr limit offset createdAt createdBefore createdAfter authors categories sorting = do
  offsetFilters <- getOffsetFilters
  let params = getFetchNewsParams limit offsetFilters createdAt createdBefore createdAfter authors categories sorting 
  dbNews <- runDB connStr $ getNewsDB params
  images <- runDB connStr $ getImagesByNewsDB dbNews
  return $ getNewsProxy dbNews images

  where -- we have validate Offest Keys
        getOffsetFilters :: Handler [Filter News] 
        getOffsetFilters = do
          case (sorting,offset) of
            -- when paginate sorted query we have to have both primary and sorting keys
            ((Just s),(Just offs)) ->
              case Text.splitOn ":" offs of 
                (primaryKey:sortingKey:[]) -> liftIO (print (primaryKey,sortingKey)) >> (validateOffestKeys primaryKey sortingKey s)
                _ -> throwError err400 { errBody = Aeson.encode ("Offset for sorted query has to contain both sorting key and primary key." :: Text ) }
            
            -- offset without sorting means offset key has to be a valid primary key
            (Nothing,(Just offs)) -> do
              liftIO $ print offs
              primKey <- parseNewsId offs
              return [NewsNewsId >. primKey]
            
            -- without offset we`re done here
            (_,Nothing) -> return []
        
        -- lets make sure we got correct primary and sorting key
        validateOffestKeys :: Text -> Text -> Text -> Handler [Filter News]
        validateOffestKeys primaryKey sortingKey sortingField = do
          parsedPrimaryKey <- parseNewsId primaryKey
          sortingFieldFilter <- case sortingField of
                                  "createdAt"     -> parseDate sortingKey >>= \day -> return $  [NewsCreation_date >. day] ||. [NewsCreation_date ==. day , NewsNewsId >. parsedPrimaryKey ]
                                  "author"        -> return $ [NewsAuthor >. sortingKey] ||. [NewsAuthor ==. sortingKey , NewsNewsId >. parsedPrimaryKey ]
                                  "category"      -> return $ [NewsCategory >. sortingKey] ||. [NewsCategory ==. sortingKey , NewsNewsId >. parsedPrimaryKey ]
                                  _ -> throwError err400 { errBody = Aeson.encode ("Unknown sorting key: \"" <> sortingField <> "\"")}
          return sortingFieldFilter

        parseNewsId :: Text -> Handler Int
        parseNewsId text = case readMaybe $ unpack text :: Maybe Int of 
                            Just id -> return id
                            Nothing -> throwError err400 { errBody = Aeson.encode ("Could not parse news id: \"" <> text <> "\".")}

        parseDate :: Text -> Handler Day
        parseDate text = case readMaybe $ unpack text :: Maybe Day of 
                            Just day -> return day
                            Nothing -> throwError err400 { errBody = Aeson.encode ("Could not parse date: \"" <> text <> "\".")}




getFetchNewsParams :: 
  Maybe Int -> [Filter News] -> Maybe Day -> Maybe Day -> Maybe Day -> [Text] -> [Text] -> Maybe Text
  -> SelectParams News
getFetchNewsParams limit offsetFilters mbcreatedAt mbcreatedBefore mbcreatedAfter authors categories sorting = 
  let createdAt = case mbcreatedAt of 
                    Just day -> [NewsCreation_date ==. day]
                    Nothing -> []
      createdBefore = case mbcreatedBefore of 
                        Just day -> [NewsCreation_date <. day]
                        Nothing -> []
      createdAfter = case mbcreatedAfter of 
                       Just day -> [NewsCreation_date >. day]
                       Nothing -> []
      authorsFilter = case authors of
                        [] -> [] 
                        authorsList -> [NewsAuthor <-. authors]
      categoryFilter = case categories of 
                         [] -> []
                         categoriesList -> [NewsCategory <-. categories]                  
                           
      filters = createdAt ++ createdBefore ++ createdAfter ++ authorsFilter ++ categoryFilter ++ offsetFilters
      
      sortingOptions = case sorting of 
                  Just "createdAt" -> [Asc NewsCreation_date,Asc NewsNewsId]
                  Just "author" -> [Asc NewsAuthor,Asc NewsNewsId]
                  Just "category" -> [Asc NewsCategory,Asc NewsNewsId]
                  Nothing -> [Asc NewsNewsId]

      limitOptions = case limit of
                        Just lim -> [LimitTo lim]
                        Nothing -> []
                        
      options = sortingOptions ++ limitOptions

  in SelectParams {..}
  
  

getNewsDB :: (MonadIO m) => SelectParams News -> SqlPersistT m [News]
getNewsDB SelectParams{..} = fmap fromEntities $ selectList filters options


-- getNewsDB :: (MonadIO m, MonadLogger m) => SqlReadT m [News]
-- getNewsDB = do

  -- Esqueleto examples
  -- newsList <- E.select $ do
        -- news <-
          -- E.from $
            -- E.table @News
        -- return news
        
  -- date <- liftIO $ liftM (toGregorian . utctDay) getCurrentTime
        
  -- let whereClause = \news -> True --news ^. NewsCreation_date E.==. (  E.Value date)
        
  -- newsAndCount <- E.select $ do
                  -- (news E.:& image) <-
                    -- E.from $ E.table @News
                    -- `E.innerJoin` E.table @Image
                    -- `E.on` (\(news E.:& image) ->
                             -- news ^. NewsNewsId  E.==. image ^. ImageNews_id)
                    -- E.where_ ( whereClause @News )
                  -- return (news, image) 

  
  -- liftIO $ print newsAndCount 
  -- return $ fromEntities newsList



postPhotosHandler :: ConnectionString -> MultipartData Mem -> Handler Bool
postPhotosHandler connStr multipartData = do

  newsID <- validateNewsId multipartData

  let contents = Prelude.map (LBS.toStrict . fdPayload) $ files multipartData

  forM_ contents $ \content -> do
    id <- liftIO randomIO 
    let image = Image { imageNews_id = newsID, imageImage_id = id, imageContent = content }
    runDB connStr $ insert image

    -- liftIO $ putStrLn "Inputs:"
    -- forM_ (inputs multipartData) $ \input ->
      -- liftIO $ putStrLn $ "  " ++ show (iName input)++ " -> " ++ show (iValue input)

  return True
  
  where validateNewsId :: MultipartData Mem -> Handler Int
        validateNewsId multipartData = do
        
          key_present <- case lookupInput "news_id" multipartData of
                            Right newsID -> return newsID :: Handler Text
                            _ -> throwError err400 { errBody = Aeson.encode ("Expected NewsID in reqest body." :: Text)}
          
          case readMaybe $ unpack key_present :: Maybe Int of
            Just int -> return int
            _ -> throwError err400 { errBody = Aeson.encode ("Could not parse \"" <> key_present <> "\" as int.")}




getImagesByNewsDB :: (MonadIO m) => [News] -> SqlPersistT m [Image]
getImagesByNewsDB news =
  let newsIDs = Prelude.map newsNewsId news
  in fmap fromEntities ( selectList [ImageNews_id <-. newsIDs] [] )
  
  
  
  
getImagesHandler :: ConnectionString -> Int -> Handler Image
getImagesHandler connStr imageID = do
  image <- runDB connStr $ selectList [ImageImage_id ==. imageID] []
  case fromEntities image of
    [image] -> return image
    _ -> throwError err404 { errBody = Aeson.encode ("Not Found" :: Text)}
	
	
	
	
getImagesByNewsHandler :: ConnectionString -> Int -> Handler [Image]
getImagesByNewsHandler connStr newsID = runDB connStr $ fmap fromEntities $ selectList [ImageNews_id ==. newsID] []