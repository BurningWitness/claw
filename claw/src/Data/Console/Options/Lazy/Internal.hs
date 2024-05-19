{-# LANGUAGE BangPatterns
           , ExistentialQuantification
           , UnboxedTuples #-}

module Data.Console.Options.Lazy.Internal
  ( Options (..)

  , empty

  , Data.Console.Options.Lazy.Internal.lookup

  , insert

  , suggestions
  ) where

import           Data.Console.Option.Internal
import qualified Data.Primitive.ByteArray.Levenshtein as Levenshtein
import           System.Console.Options.Internal
import           System.OsString.Custom

import           Control.Monad.ST
import           Data.Coerce
import           Data.Functor.Identity
import           Data.Patricia.Word.Lazy (Patricia)
import qualified Data.Patricia.Word.Lazy as Patricia
import           Data.Primitive.ByteArray
import           Data.Primitive.PrimArray
import           Data.Radix1Tree.Word8.Key.Unsafe
import           Data.Radix1Tree.Word8.Lazy (Radix1Tree)
import qualified Data.Radix1Tree.Word8.Lazy as Radix
import qualified Data.Radix1Tree.Word8.Lazy.Unsafe as Radix
import qualified System.OsString as Os
import           System.OsString.Internal.Types



-- | Spine-lazy option dictionary.
data Options m f =
       Options
         (Patricia   (m (Pair f)))
         (Radix1Tree (m (Pair f)))



empty :: Options m f
empty = Options Patricia.empty Radix.empty



{-# INLINEABLE lookup #-}
lookup :: Name -> Options Identity f -> Maybe (Pair f)
lookup name (Options pat radix) =
  coerce $
    case name of
      Short s -> Patricia.lookup (fromIntegral $ fromEnum s) pat
      Long ls -> Radix.lookup (unsafeFeedShortByteString (unify ls)) radix



insert :: Option Identity help f -> Options Identity f -> Options Identity f
insert (Option names flavor) = \options -> foldr go options names
  where
    !pair = demote flavor

    go name =
      case name of
        Short c -> insertShort c pair
        Long bs -> insertLong bs pair



insertShort
  :: Char
  -> Pair f
  -> Options Identity f
  -> Options Identity f
insertShort c pair (Options pat radix) =
  let pat' = Patricia.insert (fromIntegral $ fromEnum c) (Identity pair) pat
  in Options pat' radix

insertLong
  :: OsString
  -> Pair f
  -> Options Identity f
  -> Options Identity f
insertLong name !pair (Options pat radix) =
  let !(# radix' #)
        | Os.null name = (# radix #)
        | otherwise   =
            (# Radix.insert (unsafeFeedShortByteString (unify name)) (Identity pair) radix #)

  in Options pat radix'



-- | Fuzzy search for 'System.Console.Options.Unrecognized' options.
suggestions
  :: OsString           -- ^ Argument that the option name is a part of, without
                        --   the preceding dash(es).
  -> Name               -- ^ Option name in question.
  -> Options Identity f
  -> [Name]
suggestions arg name (Options pat radix) =
  makeSuggestions
    (\c -> Patricia.member (fromIntegral $ fromEnum c) pat)
    (\cap input -> collectLevenshtein cap input radix)
    arg
    name



collectLevenshtein :: Int -> OsString -> Radix1Tree a -> [Name]
collectLevenshtein cap input =
  let !r = runST $ Levenshtein.initialize input

  in go 0 (r :: PrimArray Int) Lin
  where
    go !i !r b t =
      case t of
        Radix.Bin _ dl dr   -> go i r b dl ++ go i r b dr
        Radix.Tip arr mx dx ->
          let !v = runST $ do _r <- thawPrimArray r 0 (sizeofPrimArray r)
                              Levenshtein.iterate i arr input _r

              more = go (i + sizeofByteArray arr) v (Snoc b arr) dx

          in if Levenshtein.rate input v <= cap
               then case mx of
                      Just _  ->
                        Long (divide $ Radix.buildShortByteString (Build1 $ b :/ arr))
                          : more

                      Nothing -> more

               else more

        Radix.Nil           -> []
