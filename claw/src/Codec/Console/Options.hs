{- | Command-line argument decoder.
 -}

{-# LANGUAGE BangPatterns #-}

module Codec.Console.Options
  ( -- * Decode
    Decoder (..)
  , ShortDecoder (..)
  , decode
  ) where

import           Data.Bits
import           Data.ByteString.Short (ShortByteString (..))
import qualified Data.ByteString.Short as SB
import           Data.Coerce
import           Data.Primitive.ByteArray
import           Data.Text.Short (ShortText)
import qualified Data.Text.Short.Unsafe as ST
import           Data.Word
import           GHC.Base (unsafeChr)
import           System.OsString.Internal.Types



-- | Argument decoder.
data Decoder = -- | Short option group.
               Short
                 !Char            -- ^ Option name.
                 (Maybe OsString) -- ^ Option argument as remainder of the group.
                 ShortDecoder     -- ^ Next short option in the group.

               -- | Long option.
             | Long
                 !ShortText        -- ^ Option name.
                 !(Maybe OsString) -- ^ Option argument.

               -- | Non-option argument.
             | Argument

               -- | End of options (@--@).
             | End
                 !(Maybe OsString) -- ^ @--=ARG@ is still a possible
                                   --   (albeit invalid) sequence.

               -- | Argument begins with a hyphen,
               --   but some characters could not be decoded.
             | Undecodable



-- | Short option group decoder.
data ShortDecoder = -- | Next short option.
                    Short'
                      !Char            -- ^ Option name.
                      (Maybe OsString) -- ^ Option argument as remainder of the group.
                      ShortDecoder     -- ^ Next short option in the group.

                    -- | End of group.
                  | End'

                    -- | Short option character could not be decoded.
                  | Undecodable'



-- | Check that the byte is in POSIX Portable Character Set.
isPortable :: Word8 -> Bool
isPortable w = w == 0 || (w >= 0x07 && w <= 0x0D) || (w >= 0x20 && w <= 0x7E)



{-
Note [Slicing raw command-line arguments]
-----------------------------------------
Per POSIX specification we can't really know what encoding
any particular OsString comes in, that's why the entire
roundtripping locale reencoding (decodeFS) process is in place.
Thankfully the specification also states each encoded portable
character is guaranteed to take up only one byte (Chapter 6.1),
for that reason we only work with the portable set.

Under Windows the encoding is guaranteed to be UTF-16[LE],
all portable characters there are two bytes wide.
-}



-- | Interpret a single argument.
--
--   Decoded option names are guaranteed to only contain characters in
--   the POSIX Portable Character Set.
--
--   Requires the same argument in two forms.
decode
  :: Bool            -- ^ @True@ for Windows, @False@ for POSIX.
  -> ByteArray       -- ^ Unwrapped @WindowsString@/@PosixString@.
  -> ShortByteString -- ^ WTF-8 byte array (same encoding as 'FilePath').
  -> Decoder
decode isWindows raw (ShortByteString arr) =
  if len < 2
    then Argument
    else
         case indexByteArray arr 0 :: Word8 of
           0x2D ->
             case indexByteArray arr 1 :: Word8 of
               0x2D
                 | len == 2  -> End Nothing
                 | otherwise ->
                     case indexByteArray arr 2 :: Word8 of
                       0x3D -> End (Just $! cutArgument 3)
                       _    -> decodeLong

               _ -> case decodeShort 1 of
                      Short' w rest next -> Short w rest next
                      Undecodable'       -> Undecodable
                      End'               -> Undecodable -- impossible

           _    -> Argument
  where
    len = sizeofByteArray arr


    ShortByteString arr2 = SB.drop 2 (ShortByteString arr)

    decodeLong =
      case SB.elemIndex 0x3D (ShortByteString arr2) of
        Nothing
          | SB.all isPortable (ShortByteString arr2) ->
              let !long = ST.fromShortByteStringUnsafe (ShortByteString arr2)
              in Long long Nothing

          | otherwise -> Undecodable

        Just i
          | SB.all isPortable $ SB.take i (ShortByteString arr2) ->
              let !long = ST.fromShortByteStringUnsafe $
                            SB.take i (ShortByteString arr2)

              in Long long (Just $! cutArgument (i + 3))

          | otherwise -> Undecodable


    decodeShort i
      | i >= len  = End'
      | otherwise =
          let w = indexByteArray arr i
          in if not $ isPortable w
               then Undecodable'
               else
                    let c = unsafeChr $ fromIntegral w

                        !i' = i + 1

                        next = decodeShort i'

                        rest | i' == len = Nothing
                             | otherwise = Just $! cutArgument i'

                    in Short' c rest next

    -- See Note [Slicing raw command-line arguments]
    cutArgument i =
      let !offset | isWindows = unsafeShiftL i 1
                  | otherwise = i

      in coerce $ SB.drop offset (ShortByteString raw)
