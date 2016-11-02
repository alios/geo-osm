{-# LANGUAGE TemplateHaskell #-}


-- | Values with a @lat@ string accessor.
module Data.Geo.OSM.Lens.LatL where

import Control.Lens.TH
import Control.Lens.Prism
import Control.Lens.Fold
import Control.Lens.Review
import Text.XML.HXT.Arrow.Pickle


newtype Latitude =
  Latitude Double
  deriving (Eq, Ord)

makeClassy ''Latitude

_Latitude :: Prism' Double Latitude
_Latitude = prism' f t
  where f (Latitude l) = l
        t l
          | l > -900 && l <= 90 = pure . Latitude $ l
          | otherwise = Nothing

_LatitudeStr :: Prism' String Latitude
_LatitudeStr = prism' f t
  where f (Latitude l) = show l
        t a = case reads a of
          [] -> Nothing
          ((l,_):_) -> preview _Latitude l

instance XmlPickler Latitude where
  xpickle = xpWrapMaybe (preview _LatitudeStr, review _LatitudeStr) xpText
