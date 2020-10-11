{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Finance
import Data.Time
import Data.Time.Calendar


parseDay :: String -> Day
parseDay = parseTimeOrError True defaultTimeLocale "%Y-%m-%d"

transaction :: Transaction
transaction = Transaction "653fc2a9-14b9-4318-bcb3-178c59458f61" "test" "test" "credit" "chase_kari" "" "cleared" 1013 1 True True (parseDay "2020-12-31") 0.0

spec :: Spec
spec =
    describe "Describe group of tests" $ do
      it "describe the test" $
        0 `shouldBe` 0

main :: IO ()
main = hspec spec
