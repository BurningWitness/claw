module System.OsString.Custom
  ( unify
  , divide
  , numWord
  , doubleton
  , unsafeSlice
  ) where

import           Control.Monad.ST
import           Data.ByteString.Short
import           Data.Primitive.ByteArray
import           Data.Type.Coercion
import qualified System.OsString as Os
import           System.OsString.Data.ByteString.Short.Word16 as Os16
import           System.OsString.Internal.Types



{-# INLINE unify #-}
unify :: OsString -> ShortByteString
unify =
  case coercionToPlatformTypes of   
    Left (_, coercion)  -> getWindowsString . coerceWith coercion
    Right (_, coercion) -> getPosixString   . coerceWith coercion

{-# INLINE divide #-}
divide :: ShortByteString -> OsString
divide =
  case coercionToPlatformTypes of   
    Left (_, coercion)  -> coerceWith (sym coercion) . WindowsString
    Right (_, coercion) -> coerceWith (sym coercion) . PosixString



{-# INLINE numWord #-}
numWord :: OsString -> Int
numWord =
  case coercionToPlatformTypes of
    Left (_, coercion) -> Os16.numWord16 . getWindowsString . coerceWith coercion
    Right _            -> Os.length



doubleton :: OsChar -> OsChar -> OsString
doubleton a b =
   case coercionToPlatformTypes of
     Left (coercion, coercion2) ->
       let WindowsChar c = coerceWith coercion a
           WindowsChar d = coerceWith coercion b

       in coerceWith (sym coercion2) . (\(ByteArray _brr) -> WindowsString (SBS _brr)) $
            runST $ do
              marr <- newByteArray 4
              writeByteArray marr 0 c
              writeByteArray marr 1 d
              unsafeFreezeByteArray marr
     
     Right (coercion, coercion2) ->
       let PosixChar c = coerceWith coercion a
           PosixChar d = coerceWith coercion b

       in coerceWith (sym coercion2) . (\(ByteArray _brr) -> PosixString (SBS _brr)) $
            runST $ do
              marr <- newByteArray 2
              writeByteArray marr 0 c
              writeByteArray marr 1 d
              unsafeFreezeByteArray marr



unsafeSlice :: Int -> Int -> OsString -> OsString
unsafeSlice at len = \os ->
  case coercionToPlatformTypes of
    Left (_, coercion) ->
      let arr = (\(WindowsString (SBS _arr)) -> ByteArray _arr) $ coerceWith coercion os
      in coerceWith (sym coercion) . (\(ByteArray _brr) -> WindowsString (SBS _brr)) $
           runST $ do
             mbrr <- newByteArray (len * 2)
             copyByteArray mbrr 0 arr (at * 2) (len * 2)
             unsafeFreezeByteArray mbrr

    Right (_, coercion) ->
      let arr = (\(PosixString (SBS _arr)) -> ByteArray _arr) $ coerceWith coercion os
      in coerceWith (sym coercion) . (\(ByteArray _brr) -> PosixString (SBS _brr)) $
           runST $ do
             mbrr <- newByteArray len
             copyByteArray mbrr 0 arr at len
             unsafeFreezeByteArray mbrr
