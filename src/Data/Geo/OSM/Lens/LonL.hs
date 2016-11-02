{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Values with a @lon@ string accessor.
module Data.Geo.OSM.Lens.LonL (Longitude, _Longitude, HasLongitude(..) ) where

import Control.Lens.Fold
import Control.Lens.Review
import Control.Lens.Prism
import Control.Lens.TH
import Data.Geo.OSM.Lens.Internal
import Text.XML.HXT.Arrow.Pickle

newtype Longitude =
  Longitude Double
  deriving (Eq, Ord)

makeClassy ''Longitude

_Longitude :: Prism' Double Longitude
_Longitude = prism' f t
  where f (Longitude l) = l
        t l
          | l > -180 && l <= 180 = pure . Longitude $ l
          | otherwise = Nothing

_LongitudeStr :: Prism' String Longitude
_LongitudeStr = showReadPrism _Longitude


instance XmlPickler Longitude where
  xpickle = xpWrapMaybe (preview _LongitudeStr, review _LongitudeStr) xpText
