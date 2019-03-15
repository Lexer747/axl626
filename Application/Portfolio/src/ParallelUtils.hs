module ParallelUtils (
        parallelMap,
        parallelMapMaybe
    ) where

import Control.Parallel.Strategies
import Data.Maybe (mapMaybe)

parallelMapMaybe :: (NFData b) => (a -> Maybe b) -> [a] -> [b]
parallelMapMaybe f xs = let ys = mapMaybe f xs
                            zs = ys `using` parList rdeepseq
                        in zs

parallelMap :: (NFData b) => (a -> b) -> [a] -> [b]
parallelMap f xs = let ys = map f xs
                       zs = ys `using` parList rdeepseq
                   in zs