{-# LANGUAGE BangPatterns
           , FlexibleInstances
           , TemplateHaskellQuotes
           , TypeApplications #-}

module Data.Console.Options.Internal
  ( OptionsQ
  , Options (..)

  , empty

  , sequenceCode

  , lookupShort
  , lookupLong

  , insert
  , insertQ

  , suggestions
  ) where

import           Data.Console.Option.Internal
import qualified Data.Primitive.ByteArray.Levenshtein as Levenshtein

import           Control.Monad
import           Control.Monad.ST
import           Data.ByteString.Short (ShortByteString (..))
import qualified Data.ByteString.Short as SB
import           Data.Char
import           Data.Coerce
import           Data.Foldable
import           Data.Functor.Identity
import qualified Data.List as List
import           Data.Patricia.Word.Strict (Patricia)
import qualified Data.Patricia.Word.Strict as Patricia
import qualified Data.Patricia.Word.Strict.TH as Patricia
import           Data.Primitive.ByteArray
import           Data.Primitive.PrimArray
import           Data.Radix1Tree.Word8.Key
import           Data.Radix1Tree.Word8.Key.Unsafe
import           Data.Radix1Tree.Word8.Strict (Radix1Tree)
import qualified Data.Radix1Tree.Word8.Strict as Radix
import qualified Data.Radix1Tree.Word8.Strict.TH as Radix
import qualified Data.Radix1Tree.Word8.Strict.Unsafe as Radix
import           Data.Text.Short (ShortText)
import qualified Data.Text.Short as ST
import qualified Data.Text.Short.Unsafe as ST
import           GHC.Base (unsafeChr)
import           Language.Haskell.TH.Syntax hiding (Name)



-- | Convenience synonym.
type OptionsQ f = Q (Options (Code Q) f)

-- | Spine-strict option tree.
data Options m f =
       Options
         (Patricia   (m (Function f)))
         (Radix1Tree (m (Function f)))

instance Show (Options Identity f) where
  showsPrec d (Options pat radix) =
    showParen (d > 10) $
      showString "Options " .
        showCurlyList
          (Patricia.foldrWithKey short (Radix.foldrWithKey long [] radix) pat)
    where
      defun f =
        case f of
          Zero  _ -> "plain"
          Maybe _ -> "optional"
          One   _ -> "required"

      short key (Identity f) =
        (:) $
          showString "-" . showChar (unsafeChr $ fromIntegral key)
                         . showString " = " . showString (defun f)

      long key (Identity f) =
        (:) $
          let txt = ST.unpack . ST.fromShortByteStringUnsafe $ buildShortByteString key
          in showString "--" . showString txt
                             . showString " = " . showString (defun f)

showCurlyList :: [ShowS] -> ShowS
showCurlyList = \xs ->
  case xs of
    []   -> showString "{}"
    x:ys -> showChar '{' . go1 x ys . showChar '}'
  where
  go1 x ys =
    x . case ys of
          []   -> id
          y:zs -> showString ", " . go1 y zs




-- | Empty option tree.
empty :: Options m f
empty = Options Patricia.empty Radix.empty



-- | Precompile the option tree.
sequenceCode :: OptionsQ f -> Code Q (Options Identity f)
sequenceCode q =
  joinCode $ do
    Options pat radix <- q
    pure
      [|| Options
            ((coerce :: Patricia   (Function f) -> Patricia   (Identity (Function f)))
               $$(Patricia.sequenceCode pat))

            ((coerce :: Radix1Tree (Function f) -> Radix1Tree (Identity (Function f)))
               $$(Radix.sequenceCode radix))
       ||]



lookupShort :: Char -> Options Identity f -> Maybe (Function f)
lookupShort s (Options pat _radix) =
  coerce $
    Patricia.lookup (fromIntegral $ fromEnum s) pat

lookupLong :: ShortText -> Options Identity f -> Maybe (Function f)
lookupLong ls (Options _pat radix) =
  coerce $
    Radix.lookup (unsafeFeedShortByteString $ ST.toShortByteString ls) radix



-- | Insert an option into the tree.
--
--   If the option has an empty list of names, nothing is added to the tree.
--   Empty long names are similarly treated as no-ops.
--   When inserting duplicate names new values take precedence.
insert
  :: Option help f arg
  -> arg
  -> Options Identity f
  -> Options Identity f
insert (Option names flavor) f = \options -> foldl' go options names
  where
    !fun = demote flavor f

    go opts name =
      case name of
        Short s -> insertShort s fun opts
        Long ls -> insertLong ls fun opts

-- | Insert an option into the tree at compilation time.
--
--   Emits warnings if any of the provided option names are inaccessible, overwrite
--   previously added names, or contain non-portable characters.
--   Operates identically to 'insert' otherwise.
insertQ
  :: Option help f arg
  -> Code Q arg
  -> OptionsQ f
  -> OptionsQ f
insertQ (Option names flavor) f = \optionsQ -> do
  options <- optionsQ
  foldlM go options names
  where
    !fun = demoteQ flavor f

    go opts name =
      case name of
        Short s -> insertShortQ s fun opts
        Long ls -> insertLongQ ls fun opts



-- | Check that the character is in POSIX Portable Character Set,
--   and is not a hyphen.
isValidShort :: Char -> Bool
isValidShort c =
      c == '\0'
  || (c >= '\x07' && c <= '\x0D')
  || (c >= '\x20' && c <= '\x2C')
  || (c >= '\x2E' && c <= '\x7E')

-- | Check that the character is in POSIX Portable Character Set,
--   and is not equals.
isValidLong :: Char -> Bool
isValidLong c =
      c == '\0'
  || (c >= '\x07' && c <= '\x0D')
  || (c >= '\x21' && c <= '\x3C')
  || (c >= '\x3E' && c <= '\x7E')



insertShort :: Char -> Function f -> Options Identity f -> Options Identity f
insertShort = coerce (insertShort_ @Identity)

insertShortQ
  :: Char
  -> Code Q (Function f)
  -> Options (Code Q) f
  -> Q (Options (Code Q) f)
insertShortQ c pair options@(Options pat _radix) = do
  let w = fromIntegral $ fromEnum c

      optionNameS = showString "Option \"-" . showChar c . showChar '"'

  when (Patricia.member w pat) $
    reportWarning $
      optionNameS " is mentioned multiple times"

  unless (isValidShort c) $
    reportWarning $
      optionNameS " uses a non-portable character or '-'"

  pure $! insertShort_ c pair options

insertShort_ :: Char -> m (Function f) -> Options m f -> Options m f
insertShort_ c pair (Options pat radix) =
  let pat' = Patricia.insert (fromIntegral $ ord c) pair pat
  in Options pat' radix



insertLong :: ShortText -> Function f -> Options Identity f -> Options Identity f
insertLong = coerce (insertLong_ @Identity)

insertLongQ
  :: ShortText
  -> Code Q (Function f)
  -> Options (Code Q) f
  -> Q (Options (Code Q) f)
insertLongQ txt pair options@(Options _pat radix) = do
  let optionNameS =
        showString "Option \"--" . showString (ST.unpack txt) . showChar '"'

  if ST.null txt
    then reportWarning $
           optionNameS " is inaccessible"

    else do
      when (Radix.member (unsafeFeedShortByteString $ ST.toShortByteString txt)
                                                                           radix) $
        reportWarning $
          optionNameS " is mentioned multiple times"

      unless (ST.all isValidLong txt) $
        reportWarning $
          optionNameS " uses non-portable characters or '='"

  pure $! insertLong_ txt pair options

insertLong_ :: ShortText -> m (Function f) -> Options m f -> Options m f
insertLong_ txt pair (Options pat radix) =
  let radix'
        | ST.null txt = radix
        | otherwise   = Radix.insert
                          (unsafeFeedShortByteString $ ST.toShortByteString txt)
                          pair radix
  in Options pat radix'



collectLevenshtein :: Int -> ShortText -> Radix1Tree a -> [ShortText]
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
                      Nothing -> more
                      Just _  ->
                        let !txt = ST.fromShortByteStringUnsafe $
                                     Radix.buildShortByteString (Build1 $ b :/ arr)

                        in txt : more

               else more

        Radix.Nil           -> []



goldenRatio :: Float
goldenRatio = 1.618034

-- | Fuzzy search for 'System.Console.Options.Unrecognized' options.
suggestions
  :: Options Identity f
  -> ShortByteString    -- ^ Full argument as a WTF-8 byte array.
  -> Name               -- ^ Option name in question.
  -> [Name]
suggestions (Options pat radix) _sb name =
  let findShort c = Patricia.member (fromIntegral $ fromEnum c) pat

      findLongLog txt =
        let raw = ST.toShortByteString txt

            ratio = floor $ logBase goldenRatio (fromIntegral $ SB.length raw)

        in findLong ratio txt

      findLong i txt = Long <$> collectLevenshtein i txt radix

  in case name of
       Short s ->
         let raw = ST.fromShortByteStringUnsafe $ SB.drop 1 _sb

         in -- A short option or short group may be a mistyped long option
            if ST.length raw == 1
              then case ST.indexMaybe raw 0 of
                     Just c | c == s ->
                       -- find any long option 1-2 wide with the char in it
                       findLong 1 $ ST.pack [s, s]

                     _ -> []

              else -- Gate by 2 since @logBase goldenRatio 1 == 0@.
                   if ST.length raw >= 2
                     then findLongLog $ ST.drop 1 raw
                     else []

       Long ls ->
         ( let suggestShort c acc
                 | findShort c = Short c : acc
                 | otherwise   =           acc

              -- A long option may be a mistyped short option or short group
           in if ST.length ls <= 5
                then \xs -> foldr suggestShort xs . List.nub $ ST.unpack ls
                else id
         )
         $ ( let raw = ST.fromShortByteStringUnsafe $ SB.drop 2 _sb

                 collectLongs
                   | ST.length ls == 1 = findLong 1 $ ls <> ls
                   | otherwise         = findLongLog ls

                -- Argument matches are only tried when the option name
                -- doesn't match, to avoid duplicate reports
             in case collectLongs of
                  [] | ST.length raw > ST.length ls + 2 ->
                         findLongLog raw

                  _ -> collectLongs
           )
