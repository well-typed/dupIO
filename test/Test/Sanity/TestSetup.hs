module Test.Sanity.TestSetup (tests) where

import Prelude hiding (IO, (<*))

import Test.Tasty

import Test.Util.TestSetup

-- | Test the test setup
--
-- We have some IO action that is driven by a list.
--
-- o The list is allocated /within/ the scope of @testOfSize@. This means it
--   will definitely be deallocated when the test completes; this is important,
--   because otherwise if one test would use a lot of memory, /all/ tests would
--   start to fail with a 'LocalOutOfMemoryException' exception.
-- o The list is allocated /outside/ the scope of the @retry@. This means that
--   since @retry@ must keep a reference to the action to execute (in case it
--   needs to retry it), and since that action keeps a reference to the list,
--   the list cannot be GCed as the action (@count@) processes it.
--
-- This means that @testOfSize@ uses memory which is linear in the size of the
-- list. We repeat the tests twice, just to ensure that an expensive test indeed
-- does not influence later cheap tests.
--
-- NOTE: If instead of @count xs@ we wrote
--
-- > unwrapIO (evaluate $ length xs)
--
-- we would /not/ see a memory leak: if we now repeat the IO action, we just
-- try to evaluate the same pure thunk again, which at this point is just the
-- length of the list.
tests :: TestTree
tests = testGroup "Test.Sanity.TestSetup" [
      testLocalOOM "OOM.expensive" $ testOfSize 250_000
    , testCaseInfo "OK.cheap"      $ testOfSize     100
    , testLocalOOM "OOM.expensive" $ testOfSize 250_000
    , testCaseInfo "OK.cheap"      $ testOfSize     100
    ]

testOfSize :: Int -> IO String
testOfSize limit = \w0 ->
    let xs :: [Double]
        xs = [1, 2 .. fromIntegral limit]

        !(# w1, _x #) = retry (count xs <* checkMem (1 * mb)) w0

    in (# w1, "succeeded with 1MB memory limit" #)

count :: [a] -> IO Int
count = \xs w -> go 0 xs w
  where
    go :: Int -> [a] -> IO Int
    go acc []     = \w -> (# w, acc #)
    go acc (_:xs) = \w -> let acc' = acc + 1
                          in acc' `seq` go acc' xs w
