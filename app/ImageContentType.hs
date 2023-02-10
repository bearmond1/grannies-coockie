module ImageContentType where

import Servant.API.ContentTypes
import Data.Typeable
import Data.ByteString


data JPEG deriving Typeable


instance Accept JPEG where
  contentType _ = "image/jpeg"
  
  
instance MimeRender JPEG ByteString where
  mimeRender _ val = fromStrict val
  
  
instance MimeUnrender JPEG ByteString where
  mimeUnrender _ val = Right $ toStrict val