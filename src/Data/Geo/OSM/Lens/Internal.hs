{-# LANGUAGE RankNTypes #-}
module Data.Geo.OSM.Lens.Internal where

import Control.Lens.Fold
import Control.Lens.Review
import Control.Lens.Prism

showReadPrism :: (Show c, Read c) => Prism' c a -> Prism' String a
showReadPrism pr = prism' f t
  where f l = show $ review pr l
        t a = case reads a of
            [] -> Nothing
            ((l,_):_) -> preview pr l
