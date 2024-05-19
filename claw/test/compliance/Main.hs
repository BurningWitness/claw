{-# LANGUAGE OverloadedStrings
           , StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import           Data.String
import           Data.List.NonEmpty
import           System.Console.Options
import           System.OsString (OsString)
import qualified System.OsString as Os
import           Test.Hspec
import           Text.Read



deriving instance Show Name
deriving instance Show Failure
deriving instance (Show a, Show b) => Show (Result a b)
deriving instance Eq Name
deriving instance Eq Failure
deriving instance (Eq a, Eq b) => Eq (Result a b)

instance IsString OsString where
  fromString = Os.unsafeEncodeUtf



data Breaker = Help | Version | NaN
               deriving (Show, Eq)



opts :: Options Identity (Int -> Either Breaker Int)
opts =
     none
  .> Option (Short 'h' :| [Long "help"])    (plain $ \_ -> Left Help)
  .> Option (Short 'v' :| [Long "version"]) (plain $ \_ -> Left Version)

  .> Option (Short 'p'    :| []) (plain $ Right . (+101))
  .> Option (Long "plain" :| [Short 'x', Short '1']) (plain $ Right . (+102))

  .> Option (Short 'o' :| [Long "optional"]) ( optional ("NUM" :: String) $ \m s ->
                                                 case m of
                                                   Nothing -> Right $ s + 104
                                                   Just xx ->
                                                     case Os.decodeUtf xx of
                                                       Nothing ->
                                                         error "Optional argument is relayed improperly"

                                                       Just x  ->
                                                         case readMaybe x of
                                                           Nothing -> Left NaN
                                                           Just i  -> Right $ s + i
                                             )

  .> Option (Long "required" :| [Short 'r']) ( required ("NUM" :: String) $ \xx s ->
                                                 case Os.decodeUtf xx of
                                                   Nothing ->
                                                     error "Required argument is relayed improperly"

                                                   Just x  ->
                                                     case readMaybe x of
                                                       Nothing -> Left NaN
                                                       Just i  -> Right $ s + i
                                             )

test :: Order -> [OsString] -> Result Breaker Int
test order = run (decode order opts) 0



main :: IO ()
main =
  hspec $ do
    it "empty" $ do
      test Permute ["foo", "bar"] `shouldBe` Success 0 ["foo", "bar"]

    describe "break" $ do
      it "single" $ do
        test Permute ["foo", "-xh", "bar"] `shouldBe` Break Help
      it "double" $ do
        test Permute ["foo", "--version", "bar"] `shouldBe` Break Version

    describe "Plain" $ do
      it "Short" $ do
        test Permute ["foo", "-p", "bar"] `shouldBe` Success 101 ["foo", "bar"]
      it "Short group" $ do
        test Permute ["foo", "-p1x1", "bar"] `shouldBe` Success 407 ["foo", "bar"]
      it "Long" $ do
        test Permute ["foo", "--plain", "bar"] `shouldBe` Success 102 ["foo", "bar"]
      it "Oversaturated" $ do
        test Permute ["foo", "--plain=314", "bar"]
          `shouldBe` Failure (Oversaturated "plain" "314")

    describe "Optional" $ do
      it "Short/Nothing" $ do
        test Permute ["foo", "-o", "bar"] `shouldBe` Success 104 ["foo", "bar"]
      it "Short group/Nothing" $ do
        test Permute ["foo", "-ppo", "bar"] `shouldBe` Success 306 ["foo", "bar"]
      it "Long/Nothing" $ do
        test Permute ["foo", "--optional", "bar"] `shouldBe` Success 104 ["foo", "bar"]
      it "Short/Just" $ do
        test Permute ["foo", "-o103", "bar"] `shouldBe` Success 103 ["foo", "bar"]
      it "Short group/Just" $ do
        test Permute ["foo", "-x1o103", "bar"] `shouldBe` Success 307 ["foo", "bar"]
      it "Long/Just" $ do
        test Permute ["foo", "--optional=103", "bar"] `shouldBe` Success 103 ["foo", "bar"]
      it "not a number" $ do
        test Permute ["foo", "--optional=baz", "bar"] `shouldBe` Break NaN

    describe "Required" $ do
      it "Short/eq" $ do
        test Permute ["foo", "-r105", "bar"] `shouldBe` Success 105 ["foo", "bar"]
      it "Long/eq" $ do
        test Permute ["foo", "--required=105", "bar"] `shouldBe` Success 105 ["foo", "bar"]
      it "Short/next" $ do
        test Permute ["foo", "-r", "106", "bar"]
          `shouldBe` Success 106 ["foo", "bar"]
      it "Long/next" $ do
        test Permute ["foo", "--required", "106", "bar"]
          `shouldBe` Success 106 ["foo", "bar"]
      it "not a number" $ do
        test Permute ["foo", "--required", "baz", "bar"] `shouldBe` Break NaN
      it "Unsaturated/Short" $ do
        test Permute ["foo", "-r"]
          `shouldBe` Failure (Unsaturated (Short 'r'))
      it "Unsaturated/Long" $ do
        test Permute ["foo", "--required"]
          `shouldBe` Failure (Unsaturated (Long "required"))
      it "Unsaturated/delimiter" $ do
        test Permute ["foo", "-r", "--"] `shouldBe` Break NaN

    describe "Unrecognized" $ do
      it "Short" $ do
        test Permute ["foo", "-q", "bar"]
          `shouldBe` Failure (Unrecognized "q" (Short 'q'))
      it "Short/arg" $ do
        test Permute ["foo", "-pquiche", "bar"]
          `shouldBe` Failure (Unrecognized "pquiche" (Short 'q'))
      it "Long" $ do
        test Permute ["foo", "--quiche", "bar"]
          `shouldBe` Failure (Unrecognized "quiche" (Long "quiche"))
      it "Long/arg" $ do
        test Permute ["foo", "--quiche=314", "bar"]
          `shouldBe` Failure (Unrecognized "quiche=314" (Long "quiche"))

    describe "delimiter" $ do
      it "Oversaturated" $ do
        test Permute ["foo", "--=314", "bar"] `shouldBe` Failure (Oversaturated "" "314")

    describe "Order" $ do
      it "RequireOrder" $ do
        test RequireOrder ["--plain", "foo", "-r=107", "bar"]
          `shouldBe` Success 102 ["foo", "-r=107", "bar"]
      it "RequireOrder/delimiter" $ do
        test RequireOrder ["--plain", "--", "-r=107", "bar"]
          `shouldBe` Success 102 ["-r=107", "bar"]
      it "Permute/delimiter" $ do
        test RequireOrder ["--plain", "--", "-r=107", "bar"]
          `shouldBe` Success 102 ["-r=107", "bar"]
