module Test.Util (
    addNumericUnderscores
  ) where

addNumericUnderscores :: String -> String
addNumericUnderscores =
      reverse
    . aux
    . reverse
  where
    aux :: String -> String
    aux str =
        case splitAt 3 str of
          (_          , []  ) -> str
          (firstThree , rest) -> firstThree ++ "_" ++ aux rest
