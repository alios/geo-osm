{-# LANGUAGE TemplateHaskell #-}
-- | The @home@ element of a OSM file.
module Data.Geo.OSM.Home
(
  Home
, home
) where

import Text.XML.HXT.Arrow.Pickle
import Data.Geo.OSM.Lens.LatL
import Data.Geo.OSM.Lens.LonL
import Data.Geo.OSM.Lens.ZoomL


import Control.Lens.TH

-- | The @home@ element of a OSM file.
data Home = Home {
  _homeLat :: Latitude,
  _homeLon :: Longitude,
  _homeZoom :: String
  } deriving Eq
makeLenses ''Home

-- | Constructs a @home@ with lat, lon and zoom.
home ::
  Latitude -- ^ The @lat@ attribute.
  -> Longitude -- ^ The @lon@ attribute.
  -> String -- ^ The @zoom@ attribute.
  -> Home
home =
  Home

instance XmlPickler Home where
  xpickle =
    xpElem "home" (xpWrap (\(lat', lon', zoom') -> home lat' lon' zoom', \(Home lat' lon' zoom') -> (lat', lon', zoom')) (xpTriple (xpAttr "lat" xpickle) (xpAttr "lon" xpickle) (xpAttr "zoom" xpText)))

instance Show Home where
  show =
    showPickled []

instance HasLatitude Home where
  latitude = homeLat

instance HasLongitude Home where
  longitude = homeLon

instance ZoomL Home where
  zoomL = homeZoom
