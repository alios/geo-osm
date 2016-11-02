-- | Values with a @minlat@ string accessor.
module Data.Geo.OSM.Lens.MinlatL where

import Control.Lens.Lens
import Data.Geo.OSM.Lens.LatL

class HasMinLat a where
  minLat :: Lens' a Latitude
