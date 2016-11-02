-- -- | Values with a @maxlon@ string accessor.
module Data.Geo.OSM.Lens.MaxlonL where

import Control.Lens.Lens
import Data.Geo.OSM.Lens.LonL

class HasMaxLon a where
  maxLon :: Lens' a Longitude
