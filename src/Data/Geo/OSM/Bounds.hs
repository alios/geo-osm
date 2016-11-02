{-# LANGUAGE TemplateHaskell #-}
-- | The @bounds@ element of a OSM file.
module Data.Geo.OSM.Bounds
(
  Bounds
, bounds
) where

import Text.XML.HXT.Arrow.Pickle
import Data.Geo.OSM.Lens.LatL
import Data.Geo.OSM.Lens.LonL
import Data.Geo.OSM.Lens.MinlatL
import Data.Geo.OSM.Lens.MaxlatL
import Data.Geo.OSM.Lens.MinlonL
import Data.Geo.OSM.Lens.MaxlonL
import Data.Geo.OSM.Lens.OriginL
import Control.Lens.TH

-- | The @bounds@ element of a OSM file.
data Bounds = Bounds {
  _boundsMinLat :: Latitude,
  _boundsMinLon :: Longitude,
  _boundsMaxLat :: Latitude,
  _boundsMaxLon :: Longitude,
  _boundsOrigin :: Maybe String
  } deriving Eq

makeLenses ''Bounds

instance XmlPickler Bounds where
  xpickle =
    xpElem "bounds" (xpWrap (\(minlat', minlon', maxlat', maxlon', origin') -> bounds minlat' minlon' maxlat' maxlon' origin', \(Bounds minlat' minlon' maxlat' maxlon' origin') -> (minlat', minlon', maxlat', maxlon', origin'))
                              (xp5Tuple (xpAttr "minlat" xpickle) (xpAttr "minlon" xpickle) (xpAttr "maxlat" xpickle) (xpAttr "maxlon" xpickle) (xpOption (xpAttr "origin" xpText))))

instance Show Bounds where
  show =
    showPickled []

instance HasMinLat Bounds where
  minLat = boundsMinLat

instance HasMinLon Bounds where
  minLon = boundsMinLon

instance HasMaxLat Bounds where
  maxLat = boundsMaxLat

instance HasMaxLon Bounds where
  maxLon = boundsMaxLon

instance OriginL Bounds where
  originL = boundsOrigin

-- | Constructs a bounds with a minlat, minlon, maxlat, maxlon and origin attributes.
bounds ::
  Latitude -- ^ The @minlat@ attribute.
  -> Longitude -- ^ The @minlon@ attribute.
  -> Latitude -- ^ The @maxlat@ attribute.
  -> Longitude -- ^ The @maxlon@ attribute.
  -> Maybe String -- ^ The @origin@ attribute.
  -> Bounds
bounds =
  Bounds
