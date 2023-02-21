module Main (main) where

import Test.Tasty

import qualified Test.Average
import qualified Test.Dup.Conduit.Closed
import qualified Test.Dup.Conduit.Sink
import qualified Test.Dup.Conduit.Source
import qualified Test.DupIO.Conduit.Closed
import qualified Test.DupIO.Conduit.Sink
import qualified Test.DupIO.Conduit.Source
import qualified Test.DupIO.Conduit.Source.Bidirectional
import qualified Test.DupIO.Conduit.Source.IO
import qualified Test.DupIO.UnsafePerformIO
import qualified Test.DupIO.UnsafeInterleaveIO
import qualified Test.Sanity.TestSetup

import GHC.Debug.Stub (withGhcDebug)

main :: IO ()
main = withGhcDebug $
    defaultMain $ testGroup "dupIO" [
        testGroup "Sanity" [
            Test.Sanity.TestSetup.tests
          ]
      , testGroup "DupIO" [
            Test.DupIO.UnsafePerformIO.tests
          , Test.DupIO.UnsafeInterleaveIO.tests
          , Test.Average.tests
          , testGroup "Conduit" [
                Test.DupIO.Conduit.Source.tests
              , Test.DupIO.Conduit.Source.Bidirectional.tests
              , Test.DupIO.Conduit.Source.IO.tests
              , Test.DupIO.Conduit.Sink.tests
              , Test.DupIO.Conduit.Closed.tests
              ]
          ]
      , testGroup "Dup" [
            testGroup "Conduit" [
                Test.Dup.Conduit.Source.tests
              , Test.Dup.Conduit.Sink.tests
              , Test.Dup.Conduit.Closed.tests
              ]
          ]
      ]