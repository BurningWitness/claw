{-# LANGUAGE BangPatterns #-}

module System.Console.Options.Internal
  ( makeSuggestions
  ) where

import           Data.Console.Option.Internal

import           Data.List
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Unsafe as Text



goldenRatio :: Float
goldenRatio = 1.618034



{-# INLINE makeSuggestions #-}
makeSuggestions
  :: (Char -> Bool)
  -> (Int -> Text -> [Name])
  -> String
  -> Name
  -> [Name]
makeSuggestions hasShort suggestLong arg name =
  let collectArg txt =
        suggestLong
          (floor $ logBase goldenRatio (fromIntegral $ Text.lengthWord8 txt))
          txt

  in case name of
       Short s ->
            ( -- A short option or short group may be a mistyped long option
              case arg of
                c:_ | c == s -> -- find any long option 1-2 wide with the char in it
                                suggestLong 1 (Text.singleton s <> Text.singleton s)
                _            -> []
            )
         <> ( -- Check (length argument >= 2), since (logBase goldenRatio 1 == 0)
              case arg of
                _:_:_ -> let !txt = Text.pack arg
                         in collectArg txt
                _     -> []
            )

       Long ls ->
         let suggestShort c acc
               | hasShort c = Short c : acc
               | otherwise  =           acc

             collectLongs
               | Text.length ls == 1 = suggestLong 1 (ls <> ls)
               | otherwise           =
                   suggestLong
                     (floor $ logBase goldenRatio (fromIntegral $ Text.lengthWord8 ls))
                     ls

         in ( -- A long option may be a mistyped short option or short group
              if Text.length ls <= 5
                then foldr suggestShort [] . nub $ Text.unpack ls
                else []
            )
         <> ( -- Argument matches are only tried when the option name
              -- doesn't match, to avoid duplicate reports
              case collectLongs of
                [] | let !txt = Text.pack arg
                   , Text.lengthWord8 txt > Text.lengthWord8 ls ->
                       collectArg txt
                
                _ -> collectLongs
            )

