-- | The children elements of the @osm@ element of a OSM file.
module Data.Geo.OSM.OSMChildren where

import Text.XML.HXT.Arrow
import Text.XML.HXT.Extras
import Data.Geo.OSM.UserE
import Data.Geo.OSM.Preferences
import Data.Geo.OSM.GpxFile
import Data.Geo.OSM.Api
import Data.Geo.OSM.ChangesetE
import Data.Geo.OSM.NodeWayRelation

-- | The children elements of the @osm@ element of a OSM file.
data OSMChildren = UserE UserE | Preferences Preferences | GpxFile GpxFile | Api Api | ChangesetE ChangesetE | NWR NodeWayRelation
  deriving Eq

instance XmlPickler OSMChildren where
  xpickle = xpAlt (\r -> case r of UserE u       -> 0
                                   Preferences p -> 1
                                   GpxFile f     -> 2
                                   Api a         -> 3
                                   ChangesetE c  -> 4
                                   NWR k         -> 5) [xpWrap (UserE, \(UserE u) -> u) xpickle,
                                                        xpWrap (Preferences, \(Preferences p) -> p) xpickle,
                                                        xpWrap (GpxFile, \(GpxFile f) -> f) xpickle,
                                                        xpWrap (Api, \(Api a) -> a) xpickle,
                                                        xpWrap (ChangesetE, \(ChangesetE c) -> c) xpickle,
                                                        xpWrap (NWR, \(NWR k) -> k) xpickle]

instance Show OSMChildren where
  show = showPickled []

-- | A @user@ element.
osmUser :: UserE -> OSMChildren
osmUser = UserE

-- | A @gpx_file@ element.
osmGpxFile :: GpxFile -> OSMChildren
osmGpxFile = GpxFile

-- | A @api@ element.
osmApi :: Api -> OSMChildren
osmApi = Api

-- | A @changeset@ element.
osmChangeset :: ChangesetE -> OSMChildren
osmChangeset = ChangesetE

-- | A @node@, @way@ or @relation@ element.
osmNodeWayRelation :: NodeWayRelation -> OSMChildren
osmNodeWayRelation = NWR

-- | Folds OSM child elements (catamorphism).
foldOSMChildren
  :: (UserE -> a) -- ^ If a @user@ element.
     -> (Preferences -> a) -- ^ If a @preferences@ element.
     -> (GpxFile -> a) -- ^ If a @gpx_file@ element.
     -> (Api -> a) -- ^ If a @api@ element.
     -> (ChangesetE -> a) -- ^ If a @changeset@ element.
     -> (NodeWayRelation -> a) -- ^ If a @node@, @way@ or @relation@ element.
     -> OSMChildren
     -> a
foldOSMChildren z _ _ _ _ _ (UserE u) = z u
foldOSMChildren _ z _ _ _ _ (Preferences p) = z p
foldOSMChildren _ _ z _ _ _ (GpxFile f) = z f
foldOSMChildren _ _ _ z _ _ (Api a) = z a
foldOSMChildren _ _ _ _ z _ (ChangesetE c) = z c
foldOSMChildren _ _ _ _ _ z (NWR k) = z k
