-- | Values with a @minlon@ string accessor.
module Data.Geo.OSM.Lens.MinlonL where

import Control.Lens.Lens
import Data.Geo.OSM.Lens.LonL

class HasMinLon a where
  minLon :: Lens' a Longitude
