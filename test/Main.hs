module Main (main) where

import Test.Tasty

import qualified Test.Average
import qualified Test.Conduit.Source
import qualified Test.Sanity.TestSetup
import qualified Test.Sanity.DupIO

main :: IO ()
main = defaultMain $ testGroup "dupIO" [
      Test.Sanity.DupIO.tests
    , Test.Sanity.TestSetup.tests
    , Test.Average.tests
    , testGroup "Conduit" [
          Test.Conduit.Source.tests
        ]
    ]
