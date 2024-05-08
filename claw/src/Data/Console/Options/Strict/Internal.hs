{-# LANGUAGE BangPatterns
           , TemplateHaskellQuotes #-}

module Data.Console.Options.Strict.Internal
  ( OptionsQ
  , Options (..)

  , empty

  , toLazy

  , sequenceCode

  , Data.Console.Options.Strict.Internal.lookup

  , Verbosity (..)
  , insert

  , suggestions
  ) where

import           Data.Console.Option.Internal
import qualified Data.Console.Options.Lazy.Internal as Lazy (Options (..))
import qualified Data.Primitive.ByteArray.Levenshtein as Levenshtein
import           System.Console.Options.Internal

import           Control.Monad
import           Control.Monad.ST
import           Data.Char
import           Data.Coerce
import           Data.Foldable
import           Data.Functor.Identity
import           Data.Patricia.Word.Strict (Patricia)
import qualified Data.Patricia.Word.Strict as Patricia
import qualified Data.Patricia.Word.Strict.TH as Patricia
import           Data.Primitive.ByteArray
import           Data.Primitive.PrimArray
import           Data.Radix1Tree.Word8.Key.Unsafe
import           Data.Radix1Tree.Word8.Strict (Radix1Tree)
import qualified Data.Radix1Tree.Word8.Strict as Radix
import qualified Data.Radix1Tree.Word8.Strict.TH as Radix
import qualified Data.Radix1Tree.Word8.Strict.Unsafe as Radix
import           Data.Text (Text)
import qualified Data.Text as Text
import           Language.Haskell.TH.Syntax hiding (Name)



-- | Convenience synonym.
type OptionsQ f = Q (Options (Code Q) f)

-- | Spine-strict option dictionary.
data Options m f =
       Options
         (Patricia   (m (Pair f)))
         (Radix1Tree (m (Pair f)))



empty :: Options m f
empty = Options Patricia.empty Radix.empty



toLazy :: Options m f -> Lazy.Options m f
toLazy (Options pat radix) = Lazy.Options (Patricia.toLazy pat) (Radix.toLazy radix)



sequenceCode :: Q (Options (Code Q) f) -> Code Q (Options Identity f)
sequenceCode q =
  joinCode $ do
    Options pat radix <- q
    pure
      [|| Options
            ((coerce :: Patricia   (Pair f) -> Patricia   (Identity (Pair f)))
               $$(Patricia.sequenceCode pat))
            
            ((coerce :: Radix1Tree (Pair f) -> Radix1Tree (Identity (Pair f)))
               $$(Radix.sequenceCode radix))
       ||]



{-# INLINEABLE lookup #-}
lookup :: Name -> Options Identity f -> Maybe (Pair f)
lookup name (Options pat radix) =
  coerce $
    case name of
      Short s -> Patricia.lookup (fromIntegral $ fromEnum s) pat
      Long ls -> Radix.lookup (unsafeFeedText ls) radix



data Verbosity = Warn
               | Silent

insert
  :: Verbosity
  -> Option (Code Q) help f
  -> Options (Code Q) f
  -> Q (Options (Code Q) f)
insert verbosity (Option names flavor) = \options -> foldlM go options names
  where
    !pair = demoteQ flavor

    go opts name =
      case name of
        Short s -> insertShort verbosity s pair opts
        Long ls -> insertLong verbosity ls pair opts



insertShort
  :: Verbosity
  -> Char
  -> Code Q (Pair f)
  -> Options (Code Q) f
  -> Q (Options (Code Q) f)
insertShort verbosity c pair (Options pat radix) = do
  let w = fromIntegral $ fromEnum c

  case verbosity of
    Silent -> pure ()
    Warn   -> do
      let optionName = "Option \"-" <> [c,'"']

      when (Patricia.member w pat) $
        reportWarning $
          optionName <> " is mentioned multiple times"

      when (not (isPrint c) || isSpace c || c == '-') $
        reportWarning $
          optionName <> " uses a non-printable character, a space or '-'"

  let !pat' = Patricia.insert (fromIntegral $ fromEnum c) pair pat
  pure $ Options pat' radix



insertLong
  :: Verbosity
  -> Text
  -> Code Q (Pair f)
  -> Options (Code Q) f
  -> Q (Options (Code Q) f)
insertLong verbosity txt !pair (Options pat radix) = do
  case verbosity of
    Silent -> pure ()
    Warn   ->
      let optionName = "Option \"--" <> Text.unpack txt <> ['"']

      in if Text.null txt
           then reportWarning $
                  optionName <> " is inaccessible"

           else do
             when (Radix.member (unsafeFeedText txt) radix) $
               reportWarning $
                 optionName <> " is mentioned multiple times"

             when (Text.any (\c -> not (isPrint c) || isSpace c || c == '=') txt) $
               reportWarning $
                 optionName <> " uses non-printable characters, spaces or '='"

  let !radix'
         | Text.null txt = radix
         | otherwise     = Radix.insert (unsafeFeedText txt) pair radix

  pure $ Options pat radix'



-- | Fuzzy search for 'System.Console.Options.TH.Unrecognized' options.
suggestions
  :: String             -- ^ Argument that the option name is a part of, without
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



collectLevenshtein :: Int -> Text -> Radix1Tree a -> [Name]
collectLevenshtein cap input =
  let !r = runST $ Levenshtein.initialize input

  in go 0 r Lin
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
                      Just _  -> Long (unsafeBuildText (Build1 $ b :/ arr)) : more
                      Nothing -> more

               else more

        Radix.Nil           -> []
