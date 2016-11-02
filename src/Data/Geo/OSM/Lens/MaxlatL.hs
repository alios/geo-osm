-- | Values with a @maxlat@ string accessor.
module Data.Geo.OSM.Lens.MaxlatL where

import Control.Lens.Lens
import Data.Geo.OSM.Lens.LatL

class HasMaxLat a where
  maxLat :: Lens' a Latitude
