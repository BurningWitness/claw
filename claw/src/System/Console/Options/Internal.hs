{-# LANGUAGE BangPatterns #-}

module System.Console.Options.Internal
  ( makeSuggestions
  ) where

import           Data.Console.Option.Internal
import           System.OsString.Custom

import           Data.List
import           System.OsString (OsString)
import qualified System.OsString as Os



goldenRatio :: Float
goldenRatio = 1.618034



{-# INLINE makeSuggestions #-}
makeSuggestions
  :: (Char -> Bool)
  -> (Int -> OsString -> [Name])
  -> OsString
  -> Name
  -> [Name]
makeSuggestions hasShort suggestLong arg name =
  let collectArg =
        suggestLong
          (floor $ logBase goldenRatio (fromIntegral $ numWord arg))
          arg

  in case name of
       Short s ->
            ( let c = Os.unsafeFromChar s
              -- A short option or short group may be a mistyped long option
              in if not (Os.null arg) && Os.index arg 0 == c
                   then -- find any long option 1-2 wide with the char in it
                        suggestLong 1 (doubleton c c)

                   else []
            )
         <> ( -- Check (length argument >= 2), since (logBase goldenRatio 1 == 0)
              if numWord arg >= 2
                then collectArg
                else []
            )

       Long ls ->
         let suggestShort c acc
               | hasShort c = Short c : acc
               | otherwise  =           acc

             collectLongs
               | numWord ls == 1 = let c = Os.index ls 0
                                   in suggestLong 1 (doubleton c c)
               | otherwise       =
                   suggestLong
                     (floor $ logBase goldenRatio (fromIntegral $ numWord ls))
                     ls

         in ( -- A long option may be a mistyped short option or short group
              if numWord ls <= 5
                then foldr suggestShort [] . nub . fmap Os.toChar $ Os.unpack ls
                else []
            )
         <> ( -- Argument matches are only tried when the option name
              -- doesn't match, to avoid duplicate reports
              case collectLongs of
                [] | numWord arg > numWord ls ->
                       collectArg
                
                _ -> collectLongs
            )
