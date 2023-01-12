module Main (main) where

import Test.Tasty

import qualified Test.Average
import qualified Test.Conduit.Closed
import qualified Test.Conduit.Sink
import qualified Test.Conduit.Source
import qualified Test.Conduit.Source.Bidirectional
import qualified Test.Sanity.DupIO
import qualified Test.Sanity.TestSetup

main :: IO ()
main = defaultMain $ testGroup "dupIO" [
      Test.Sanity.DupIO.tests
    , Test.Sanity.TestSetup.tests
    , Test.Average.tests
    , testGroup "Conduit" [
          Test.Conduit.Source.tests
        , Test.Conduit.Source.Bidirectional.tests
        , Test.Conduit.Sink.tests
        , Test.Conduit.Closed.tests
        ]
    ]
