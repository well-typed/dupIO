import Data.Foldable

import Data.Dup (dupIO)

average :: [Int] -> IO Int
average xs = do
    xs' <- dupIO xs
    return $ foldl' (+) 0 xs `div` length xs'

main :: IO ()
main = print =<< average [1 .. 10_000_000]
