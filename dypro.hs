module Dypro where

import Common
import Control.Monad
  ( zipWithM
  , liftM2
  )

import Data.List
  ( foldl'
  )

type Map k v = [(k, v)]

fpts :: Int -> Map [Int] [FT]
fpts = (fptsMem !!) . (subtract 1)

fptsMem :: [Map [Int] [FT]]
fptsMem = [ fpts' n | n <- [1 ..] ]

fptsMemRev :: [Map [Int] [FT]]
fptsMemRev = [ fptsRev' n | n <- [1 ..] ] -- @fptsMemRev@ uses reversed keys

fpts' :: Int -> Map [Int] [FT]
fpts' 1 = [([0],[A])]
fpts' n = mergeList over `merge` mergeList under
  where
    prev  = take (n - 1) fptsMem
    prev' = reverse $ take (n - 1) fptsMemRev
    over  = zipWith (crossMap (\k k' -> k ++ map (subtract 1) k') (:/))
                    prev prev'
    under = zipWith (crossMap (\k k' -> map (+ 1) k ++ k') (:\))
                    prev' prev

fptsRev' :: Int -> Map [Int] [FT]
fptsRev' 1 = [([0],[A])]
fptsRev' n = mergeList over `merge` mergeList under
  where
    prev  = reverse $ take (n - 1) fptsMem
    prev' = take (n - 1) fptsMemRev
    over = zipWith (crossMap (\k k' -> map (subtract 1) k ++ k') (flip (:/)))
                   prev prev'
    under = zipWith (crossMap (\k k' -> k ++ map (+ 1) k') (flip (:/)))
                    prev' prev

mergeList :: [Map [Int] [FT]] -> Map [Int] [FT]
mergeList = foldl' merge []

merge :: Map [Int] [FT] -> Map [Int] [FT] -> Map [Int] [FT]
merge [] ys = ys
merge xs [] = xs
merge xs'@((k, v) : xs) ys'@((h, u) : ys)
  |    k == h = (k, u ++ v) : merge xs  ys
  |    k < h  = (k, v)      : merge xs  ys'
  | otherwise = (h, u)      : merge xs' ys

twice :: a -> (a, a)
twice x = (x, x)

crossMap :: (k -> k -> h) -> (v -> v -> u)
         -> [(k, [v])] -> [(k, [v])] -> [(h, [u])]
crossMap fk fv l l' = [ (fk k k', [ fv v v' | v <- vs, v' <- vs' ])
                      | (k, vs) <- l, (k', vs') <- l' ]

