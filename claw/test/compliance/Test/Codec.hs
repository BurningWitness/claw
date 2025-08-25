{-# LANGUAGE BangPatterns
           , MagicHash
           , OverloadedStrings
           , StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Codec
  ( codec
  ) where

import           Codec.Console.Options
import           Data.Coerce
import           Data.Array.Byte
import           Data.ByteString.Short (ShortByteString (..))
import           GHC.Encoding.UTF8
import           System.OsString.Internal.Types
import qualified System.OsPath as OsPath
import           Test.Hspec



deriving instance Show ShortDecoder
deriving instance Eq ShortDecoder
deriving instance Show Decoder
deriving instance Eq Decoder



unwrap :: String -> IO Decoder
unwrap str = do
  let isWindows = case coercionToPlatformTypes of
                    Left _windows -> True
                    Right _posix  -> False

  raw <- OsPath.encodeFS str

  let !sb = ShortByteString $ ByteArray (utf8EncodeByteArray# str)

  pure $! decode isWindows (coerce raw) sb



codec :: Spec
codec = do
  it " " $ do
    unwrap "" `shouldReturn` Argument

  it "-" $ do
    unwrap "-" `shouldReturn` Argument

  it "-a" $ do
    unwrap "-a" `shouldReturn` Short 'a' Nothing End'

  it "-\SUB" $ do
    unwrap "-\SUB" `shouldReturn` Undecodable

  it "-abcde" $ do
    bcde <- OsPath.encodeFS "bcde"
    cde <- OsPath.encodeFS "cde"
    de <- OsPath.encodeFS "de"
    e <- OsPath.encodeFS "e"
    unwrap "-abcde" `shouldReturn` Short 'a' (Just bcde)
                                    (Short' 'b' (Just cde)
                                      (Short' 'c' (Just de)
                                        (Short' 'd' (Just e)
                                          (Short' 'e' Nothing End'))))

  it "-abc\SUBe" $ do
    bcde <- OsPath.encodeFS "bc\SUBe"
    cde <- OsPath.encodeFS "c\SUBe"
    de <- OsPath.encodeFS "\SUBe"
    unwrap "-abc\SUBe" `shouldReturn` Short 'a' (Just bcde)
                                       (Short' 'b' (Just cde)
                                         (Short' 'c' (Just de) Undecodable'))

  it "--" $ do
    unwrap "--" `shouldReturn` End Nothing

  it "--=ARG" $ do
    arg <- OsPath.encodeFS "ARG"
    unwrap "--=ARG" `shouldReturn` End (Just arg)

  it "--this" $ do
    unwrap "--this" `shouldReturn` Long "this" Nothing

  it "--this=that" $ do
    arg <- OsPath.encodeFS "that"
    unwrap "--this=that" `shouldReturn` Long "this" (Just arg)

  it "--th\SUBs" $ do
    unwrap "--th\SUBs" `shouldReturn` Undecodable

  it "--this=th\SUBt" $ do
    arg <- OsPath.encodeFS "th\SUBt"
    unwrap "--this=th\SUBt" `shouldReturn` Long "this" (Just arg)

  it "th\SUBs" $ do
    unwrap "th\SUBs" `shouldReturn` Argument

  it "this=th\SUBt" $ do
    unwrap "this=th\SUBt" `shouldReturn` Argument
