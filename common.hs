module Common where

import Data.List ( intercalate )

data FT = A | FT :/ FT | FT :\ FT
  deriving Show

newtype LL a = LL [[a]]

instance Show a => Show (LL a) where
  show (LL l) = intercalate "\n" $ map show l

