module Dypro where

import Common
import Control.Monad
  ( zipWithM
  , liftM2
  )

import Data.List
  ( foldl'
  )

import Data.Trie
  ( Trie
  , empty
  , singleton
  , fromList
  , toList
  , mergeBy
  )

import qualified Data.ByteString as B
  ( pack
  , map
  , append
  )

type Map = Trie [FT]

fpts :: Int -> Map
fpts = (fptsMem !!) . (subtract 1)

fptsMem :: [Map]
fptsMem = [ fpts' n | n <- [1 ..] ]

fptsMemRev :: [Map]
fptsMemRev = [ fptsRev' n | n <- [1 ..] ] -- @fptsMemRev@ uses reversed keys

fpts' :: Int -> Map
fpts' 1 = singleton (B.pack [0]) [A]
fpts' n = mergeList over `merge` mergeList under
  where
    prev  = map toList $ take (n - 1) fptsMem
    prev' = map toList $ reverse $ take (n - 1) fptsMemRev
    over  = map fromList
          $ zipWith (crossMap (\k k' -> k `B.append` B.map (subtract 1) k') (:/))
                    prev prev'
    under = map fromList
          $ zipWith (crossMap (\k k' -> B.map (+ 1) k `B.append` k') (:\))
                    prev' prev

fptsRev' :: Int -> Map
fptsRev' 1 = singleton (B.pack [0]) [A]
fptsRev' n = mergeList over `merge` mergeList under
  where
    prev  = map toList $ reverse $ take (n - 1) fptsMem
    prev' = map toList $ take (n - 1) fptsMemRev
    over  = map fromList
          $ zipWith (crossMap (\k k' -> B.map (subtract 1) k `B.append` k') (flip (:/)))
                   prev prev'
    under = map fromList
          $ zipWith (crossMap (\k k' -> k `B.append` B.map (+ 1) k') (flip (:/)))
                    prev' prev

mergeList :: [Map] -> Map
mergeList = foldl' merge empty

merge :: Map -> Map -> Map
merge = (mergeBy $ (Just .) . (++))

crossMap :: (k -> k -> h) -> (v -> v -> u)
         -> [(k, [v])] -> [(k, [v])] -> [(h, [u])]
crossMap fk fv l l' = [ (fk k k', [ fv v v' | v <- vs, v' <- vs' ])
                      | (k, vs) <- l, (k', vs') <- l' ]

