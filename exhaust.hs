module Exhaust where

import Common
import Control.Monad
import Data.Function ( on )
import Data.List
  ( groupBy
  , sortBy
  )

fts :: Int -> [FT]
fts n = ft [[A]] n
  where
    ft (h : t) 1 = h
    ft      t  n = ft (h : t) (n - 1)
      where h = zip' (:/) ++ zip' (:\)
            zip' cons = concat $ zipWith (liftM2 cons) t t'
            t' = reverse t

h :: FT -> [Int]
h A = [0]
h (l :/ r) = h l ++ (reverse . map (subtract 1) $ h r)
h (l :\ r) = (reverse . map (+ 1) $ h l) ++ h r

hft n = groupBy ((==) `on` fst) $ sortBy (compare `on` fst) $ zip (map h fs) fs
  where fs = fts n
