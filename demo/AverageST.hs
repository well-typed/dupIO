import Control.Monad.ST
import Data.Foldable

import Data.Dup (dupST)

average :: [Int] -> Int
average xs = runST $ do
    xs' <- dupST xs
    return $ foldl' (+) 0 xs `div` length xs'

main :: IO ()
main = print $ average [1 .. 10_000_000]
