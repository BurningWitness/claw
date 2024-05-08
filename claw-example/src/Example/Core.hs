module Example.Core
  ( Base (..)
  , Operation (..)
  , Verbosity (..)

  , execute
  ) where

import           Control.Monad
import           Data.Char
import           Data.Traversable
import           System.Exit
import           System.IO
import           Numeric



newtype Base = Base Int

data Operation = Sum
               | Product
               | Average
               | Median

data Verbosity = Quiet
               | Verbose1
               | Verbose2



showsBase :: Base -> ShowS
showsBase (Base i) = shows i

showsOperation :: Operation -> ShowS
showsOperation op =
  case op of
    Sum     -> showString "sum"
    Product -> showString "product"
    Average -> showString "average"
    Median  -> showString "median"



print2 :: Verbosity -> String -> IO ()
print2 Verbose2 str = hPutStrLn stderr str
print2 _        _   = pure ()

print1 :: Verbosity -> String -> IO ()
print1 Quiet _   = pure ()
print1 _     str = hPutStrLn stderr str
              


execute :: Maybe Verbosity -> Maybe Base -> Maybe Operation -> [String] -> IO ()
execute mayVerbosity mayBase mayOperation args = do
  let verbosity = maybe Verbose1  id mayVerbosity
      base      = maybe (Base 10) id mayBase
      operation = maybe Sum       id mayOperation

  print2 verbosity $ showString "Using base '" $ showsBase base "'"
  print2 verbosity $ showString "Using operation '" $ showsOperation operation "'"

  execute_ verbosity base operation args

execute_ :: Verbosity -> Base -> Operation -> [String] -> IO ()
execute_ verbosity base operation args = do
  when (null args) $ do
    print1 verbosity "No numbers were provided"

  nums <- for (zip [1 :: Int ..] args) $ \(n, str) ->
            case parseNumber base str of
              Nothing -> do
                hPutStrLn stderr $
                  showChar '\'' . showString str . showString "' is not a valid base-"
                                                 $ showsBase base " number"

                exitWith (ExitFailure 1)

              Just i -> do
                print1 verbosity $
                  showString "Number #" . shows n . showString " is decimal '"
                                                                 $ shows i "'"

                pure i

  putStrLn $ show (applyOperation operation nums)



parseNumber :: Base -> String -> Maybe Int
parseNumber (Base base) str =
  let valid c = isHexDigit c && digitToInt c < base
  in case filter (null . snd) $ readInt base valid digitToInt str of
       []       -> Nothing
       (i, _):_ -> Just i



applyOperation :: Operation -> [Int] -> Double
applyOperation op nums =
  case op of
    Sum     -> fromIntegral $ sum nums
    Product -> fromIntegral $ product nums
    Average
      | length nums == 0 -> 0
      | otherwise        -> sum (fromIntegral <$> nums) / fromIntegral (length nums)

    Median
      | odd (length nums)
      , i:_ <- drop (quot (length nums) 2 - 1) nums -> fromIntegral i

      | otherwise                ->
          case drop (quot (length nums) 2 - 1) nums of
            i:j:_ -> (fromIntegral i + fromIntegral j) / 2
            _     -> 0
