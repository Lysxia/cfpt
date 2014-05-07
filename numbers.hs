module Main where

import Dypro ( fptsMem )
import Data.Trie ( size )

main = print $ take 14 $ map size fptsMem
