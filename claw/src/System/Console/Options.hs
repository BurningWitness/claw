{-# LANGUAGE BangPatterns
           , MagicHash
           , OverloadedStrings
           , UnboxedTuples #-}

{- | Command-line option parsing.

     == Implementation

     Description of the argument syntax can be found at
     [GNU Program Argument Syntax Conventions](https://www.gnu.org/software/libc/manual/html_node/Argument-Syntax.html)
     page, which extends the short-option-only
     [POSIX Utility Conventions](https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap12.html).

     On POSIX-compliant systems string comparisons rely on POSIX portable characters
     always being represented in a single byte, per
     [POSIX Character Set](https://pubs.opengroup.org/onlinepubs/9799919799/basedefs/V1_chap06.html)
     specification.
 -}

module System.Console.Options
  ( -- * Parse
    Failure (..)
  , Outcome (..)
  , parse
  ) where

import qualified Codec.Console.Options as Codec
import           Data.Console.Option.Internal
import           Data.Console.Options.Internal as Options

import           Data.ByteString.Short (ShortByteString (..))
import           Data.Coerce
import           Data.Functor.Identity
import           Data.Primitive.ByteArray
import           Data.Text.Short (ShortText)
import           GHC.Encoding.UTF8 (utf8EncodeByteArray#)
import qualified System.OsPath as OsPath
import           System.OsString.Internal.Types



-- | Argument parsing error.
data Failure = -- | Option contains characters outside of the POSIX Portable Character Set.
               NonPortable
                 !OsString        -- ^ Raw argument.
                 !ShortByteString -- ^ Argument converted to WTF-8.

             | -- | Option name isn't on the list.
               Unrecognized
                 !OsString        -- ^ Raw argument.
                 !ShortByteString -- ^ Argument converted to WTF-8.
                 !Name            -- ^ Parsed option name.

               -- | Option requires an argument, but none was provided.
             | Unsaturated
                 !Name     -- ^ Parsed option name.

               -- | Option takes no arguments, but was provided with one.
             | Oversaturated
                 !ShortText -- ^ Long option name, may be empty (consider @--=ARG@).
                 !OsString  -- ^ Argument passed to the option.



-- | Parsing outcome.
data Outcome r = -- | Reached end of options successfully.
                 --
                 --   Returns modified state and the list of non-option arguments
                 --   in the order they were encountered.
                 Success [OsString]

                 -- | Optional early exit.
               | Broken r

               | Failure !Failure



-- | Convert 't:OsString' to a WTF-8 byte array
interpret :: OsString -> IO ShortByteString
interpret raw = do
  str <- OsPath.decodeFS raw
                            -- Relying on this function to not replace surrogates
  pure $! ShortByteString $ ByteArray (utf8EncodeByteArray# str)



-- | Parse command-line options.
--
--   Treats characters outside of POSIX Portable Character Set as incomparable.
--
--   Relies on 'GHC.IO.Encoding.getFileSystemEncoding' on POSIX-compliant platforms.
parse
  :: Options Identity (s -> IO (Either r ()))
  -> s
  -> [OsString]
  -> IO (Outcome r)
parse opts s = next_ []
  where
    next keep xs out =
      case out of
        Right _ -> next_ keep xs
        Left r  -> pure $ Broken r

    next_ keep xs =
      case xs of
        []          -> pure $ let !(# rest #) = rev [] keep
                              in Success rest
        this : rest -> do
          wtf8 <- interpret this

          let saturate name f =
                case rest of
                  arg : rest' -> do
                    r <- f arg s
                    next keep rest' r

                  [] -> pure . Failure $ Unsaturated name

              nextShort c mayArg down =
                let short = Short c
                in case Options.lookupShort c opts of
                     Nothing   -> pure . Failure $ Unrecognized this wtf8 short
                     Just pair ->
                       case pair of
                         Zero f -> do
                           r <- f s
                           case r of
                             Left r' -> pure $ Broken r'
                             Right _ ->
                               case down of
                                 Codec.End'                    -> next_ keep rest
                                 Codec.Short' c' mayArg' down' ->
                                   nextShort c' mayArg' down'

                                 Codec.Undecodable' ->
                                   pure . Failure $ NonPortable this wtf8

                         Maybe f -> do
                           r <- f mayArg s
                           next keep rest r

                         One f ->
                           case mayArg of
                             Just arg -> do
                               r <- f arg s
                               next keep rest r

                             Nothing -> saturate short f

              isWindows = case coercionToPlatformTypes of
                            Left _windows -> True
                            Right _posix  -> False

          case Codec.decode isWindows (coerce this) wtf8 of
            Codec.End mayArg ->
              case mayArg of
                Just arg -> pure . Failure $ Oversaturated "" arg
                Nothing  -> pure $ let !(# rest' #) = rev rest keep
                                   in Success rest'

            Codec.Argument -> next_ (this : keep) rest

            Codec.Undecodable -> pure . Failure $ NonPortable this wtf8

            Codec.Short c mayArg down -> nextShort c mayArg down

            Codec.Long ls mayArg ->
              let long = Long ls
              in case Options.lookupLong ls opts of
                   Nothing   -> pure . Failure $ Unrecognized this wtf8 long
                   Just pair ->
                     case pair of
                       Zero f  ->
                         case mayArg of
                           Just arg -> pure . Failure $ Oversaturated ls arg
                           Nothing  -> do
                             r <- f s
                             next keep rest r

                       Maybe f -> do
                         r <- f mayArg s
                         next keep rest r

                       One f ->
                         case mayArg of
                           Just arg -> do
                             r <- f arg s
                             next keep rest r

                           Nothing -> saturate long f



-- | Reverse the second list and append to the first one.
rev :: [a] -> [a] -> (# [a] #)
rev zs xs =
  case xs of
    []   -> (# zs #)
    x:ys -> let !zs' = x : zs
            in rev zs' ys
