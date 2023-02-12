module Docs where


import Data.String
import Servant
import Servant.Docs
import Servant.Multipart
import API
import Data.ByteString as BS
import Handlers.UserHandlers
import Handlers.NewsHandlers
import Data.Text
import GHC.Generics
import DBTypes

import Data.Time.Calendar



apiDocs :: API
apiDocs = docs userAPI


sampleDay = ModifiedJulianDay 1998016

instance ToSample UserProxyType where
  toSamples _ = singleSample ( UserProxyType 
                               { login = "hacker228"
							   , pass = "secretPass"
							   , email = "hacker@gmail.com"
							   , phone_number = "123456"
							   , join_date = sampleDay
							   , admin = False
							   , author = False
							   } )


instance ToParam (QueryParam "createdAt" Day) where
  toParam _ = 
    DocQueryParam "createdAt"
	  ["16.01.1998"]
	  "Date when news was created"
	  Normal
	  
instance ToParam (QueryParam "createdBefore" Day) where
  toParam _ = 
    DocQueryParam "createdBefore"
	  ["16.01.1998"]
	  "Latest date when news was created"
	  Normal
	  
instance ToParam (QueryParam "createdAfter" Day) where
  toParam _ = 
    DocQueryParam "createdAfter"
	  ["16.01.1998"]
	  "Minimal date when news was created"
	  Normal


instance ToParam (QueryParam "sort_by" Text) where
  toParam _ = 
    DocQueryParam "sort_by" 
	  ["createdAt", "author", "category"]
	  "Result will be sorted by specified field in ascending order. Notice that secondary sorting field is always userLogin in ascending order, and if sort_by missing, its primary sorting field"
	  Normal

instance ToParam (QueryParams "author" Text) where
  toParam _ = 
   DocQueryParam "author"
     ["Stephen King","Thomas Mann, Hermann Hesse"]
	 "List of news authors"
	 List
	 
instance ToParam (QueryParams "category" Text) where
  toParam _ = 
    DocQueryParam "category"
	  ["news","sport, politics"]
	  "List of news categories"
	  List
	  
instance ToMultipartSample Mem (  (MultipartData Mem) ) where
  toMultipartSamples  _ = [ ]
  
-- instance Generic Int
instance ToSample Int where
  toSamples _ = singleSample 123
  
--instance Generic Text
instance ToSample Text where
  toSamples _ = singleSample "Sample Text"

instance ToSample News where
  toSamples _ = singleSample 
    News
	  { newsNewsId = 123
	  , newsCreation_date = sampleDay
	  , newsCategory = "sport"
	  , newsHeader = "New jumping record"
	  , newsContent = "We got a new record"
	  , newsIs_published = False
	  , newsAuthor = "bearmond"
	  }


instance ToSample NewsProxyType where
  toSamples _ = singleSample
     NewsProxyType 
       { newsId = 123
       , creation_date = sampleDay
       , category = "sport"
       , header = "New jumping record"
       , content = "We got a new record"
       , is_published = False
       , author = "bearmond"
       , imagesURLs = ["localhost:3000/images/123"]
       }


instance ToSample ByteString where
  toSamples _ = singleSample BS.empty

instance ToSample Image where
  toSamples _ = singleSample
    Image
	  { imageNews_id = 123
	  , imageImage_id = 123
	  , imageContent = BS.empty
	  }

instance ToSample Category where
  toSamples _ = singleSample
    Category
	  { categoryName = "olympic games"
	  , categoryParent = Just "sport"
	  }


instance ToCapture (Capture "audio_id" Int) where
  toCapture _ = DocCapture 	"audio_id" "Audio file ID"


instance ToCapture (Capture "image_id" Int) where
  toCapture _ = DocCapture 	"image_id" "Image ID"


instance ToCapture (Capture "login" Text) where
  toCapture _ =  DocCapture  "login" "User login"


instance ToCapture (Capture "news_id" Int) where
  toCapture _ = DocCapture "news_id" "News ID"


instance ToCapture (Capture "category_id" Text) where
  toCapture _ =  DocCapture "category_id" "Category ID"


printDocs :: IO ()
printDocs = Prelude.writeFile "C:\\Users\\madOr\\grannies-coockie\\api_docs.md" $ markdown apiDocs