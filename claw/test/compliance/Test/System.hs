{-# LANGUAGE DerivingStrategies
           , GeneralizedNewtypeDeriving
           , NoFieldSelectors
           , OverloadedRecordDot
           , OverloadedStrings
           , StandaloneDeriving
           , TemplateHaskellQuotes #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.System
  ( options
  , optionsQ

  , system
  ) where

import           Data.Console.Options
import           Data.Functor.Identity
import           Data.IORef
import           Data.String
import           Data.Text.Short (ShortText)
import qualified Data.Text.Short as ST
import           Language.Haskell.TH (Code, Q)
import           System.Console.Options
import           System.OsPath (decodeFS, encodeFS)
import           System.OsString.Internal.Types
import           Test.Hspec
import           Text.Read



deriving instance Show Name
deriving instance Show Failure
deriving instance Show r => Show (Outcome r)
deriving instance Eq Name
deriving instance Eq Failure
deriving instance Eq r => Eq (Outcome r)



data Breaker = Help | Version | NaN
               deriving (Show, Eq)



opt1, opt2, opt3, opt4 :: Option () f f
opt1 = Option [Short 'h', Long "help"] Plain
opt2 = Option [Short 'v', Long "version"] Plain
opt3 = Option [Short 'p'] Plain
opt4 = Option [Long "plain", Short 'x', Short '1'] Plain

opt5 :: Option () f (Maybe OsString -> f)
opt5 = Option [Short 'o', Long "optional"] (Optional ())

opt6 :: Option () f (OsString -> f)
opt6 = Option [Long "required", Short 'r'] (Required ())

fun1 :: IORef Int -> IO (Either Breaker ())
fun1 _ = pure $ Left Help

fun2 :: IORef Int -> IO (Either Breaker ())
fun2 _ = pure $ Left Version

fun3 :: IORef Int -> IO (Either Breaker ())
fun3 s = do
  modifyIORef' s (+101)
  pure $ Right ()

fun4 :: IORef Int -> IO (Either Breaker ())
fun4 s = do
  modifyIORef' s (+102)
  pure $ Right ()

fun5 :: Maybe OsString -> IORef Int -> IO (Either Breaker ())
fun5 mayArg s = do
  mayInt <- case mayArg of
              Nothing  -> pure $ Just 104
              Just arg -> do
                x <- decodeFS arg
                case readMaybe x of
                  Just i  -> pure $ Just i
                  Nothing -> pure Nothing

  case mayInt of
    Nothing -> pure $ Left NaN
    Just i  -> do
      modifyIORef' s (+i)
      pure $ Right ()

fun6 :: OsString -> IORef Int -> IO (Either Breaker ())
fun6 arg s = do
  x <- decodeFS arg
  case readMaybe x of
    Nothing -> pure $ Left NaN
    Just i  -> do
      modifyIORef' s (+i)
      pure $ Right ()



options :: Options Identity (IORef Int -> IO (Either Breaker ()))
options =
    insert opt1 fun1
  . insert opt2 fun2
  . insert opt3 fun3
  . insert opt4 fun4
  . insert opt5 fun5
  . insert opt6 fun6
  $ empty



optionsQ :: Code Q (Options Identity (IORef Int -> IO (Either Breaker ())))
optionsQ =
  sequenceCode $
      insertQ opt1 [|| fun1 ||]
    . insertQ opt2 [|| fun2 ||]
    . insertQ opt3 [|| fun3 ||]
    . insertQ opt4 [|| fun4 ||]
    . insertQ opt5 [|| fun5 ||]
    . insertQ opt6 [|| fun6 ||]
    $ pure empty




newtype Path = Path ShortText
               deriving newtype (Eq, Show, IsString)

decodePath :: OsString -> IO Path
decodePath raw = do
  str <- decodeFS raw
  pure $! Path (ST.pack str)

encodePath :: Path -> IO OsString
encodePath (Path txt) = encodeFS (ST.unpack txt)



data Outcome' s r = Success' s [Path]
                  | Broken' r
                  | Failure' Failure
                    deriving (Show, Eq)



test
  :: Options Identity (IORef Int -> IO (Either Breaker ())) -> [Path]
  -> IO (Outcome' Int Breaker)
test opts args = do
  s <- newIORef 0
  args' <- traverse encodePath args
  out <- parse opts s args'
  case out of
    Success rest -> do
      s' <- readIORef s
      rest' <- traverse decodePath rest
      pure $ Success' s' rest'

    Broken r -> pure $ Broken' r

    Failure err -> pure $ Failure' err



system :: Options Identity (IORef Int -> IO (Either Breaker ())) -> Spec
system opts = do
  it "empty" $ do
    test opts ["foo", "bar"] `shouldReturn` Success' 0 ["foo", "bar"]

  describe "break" $ do
    it "single" $ do
      test opts ["foo", "-xh", "bar"] `shouldReturn` Broken' Help
    it "double" $ do
      test opts ["foo", "--version", "bar"] `shouldReturn` Broken' Version

  describe "Plain" $ do
    it "Short" $ do
      test opts ["foo", "-p", "bar"] `shouldReturn` Success' 101 ["foo", "bar"]
    it "Short group" $ do
      test opts ["foo", "-p1x1", "bar"] `shouldReturn` Success' 407 ["foo", "bar"]
    it "Long" $ do
      test opts ["foo", "--plain", "bar"] `shouldReturn` Success' 102 ["foo", "bar"]
    it "Oversaturated" $ do
      oversaturated <- encodePath "314"
      test opts ["foo", "--plain=314", "bar"]
        `shouldReturn` Failure' (Oversaturated "plain" oversaturated)

  describe "Optional" $ do
    it "Short/Nothing" $ do
      test opts ["foo", "-o", "bar"] `shouldReturn` Success' 104 ["foo", "bar"]
    it "Short group/Nothing" $ do
      test opts ["foo", "-ppo", "bar"] `shouldReturn` Success' 306 ["foo", "bar"]
    it "Long/Nothing" $ do
      test opts ["foo", "--optional", "bar"] `shouldReturn` Success' 104 ["foo", "bar"]
    it "Short/Just" $ do
      test opts ["foo", "-o103", "bar"] `shouldReturn` Success' 103 ["foo", "bar"]
    it "Short group/Just" $ do
      test opts ["foo", "-x1o103", "bar"] `shouldReturn` Success' 307 ["foo", "bar"]
    it "Long/Just" $ do
      test opts ["foo", "--optional=103", "bar"] `shouldReturn` Success' 103 ["foo", "bar"]
    it "not a number" $ do
      test opts ["foo", "--optional=baz", "bar"] `shouldReturn` Broken' NaN

  describe "Required" $ do
    it "Short/eq" $ do
      test opts ["foo", "-r105", "bar"] `shouldReturn` Success' 105 ["foo", "bar"]
    it "Long/eq" $ do
      test opts ["foo", "--required=105", "bar"] `shouldReturn` Success' 105 ["foo", "bar"]
    it "Short/next" $ do
      test opts ["foo", "-r", "106", "bar"]
        `shouldReturn` Success' 106 ["foo", "bar"]
    it "Long/next" $ do
      test opts ["foo", "--required", "106", "bar"]
        `shouldReturn` Success' 106 ["foo", "bar"]
    it "not a number" $ do
      test opts ["foo", "--required", "baz", "bar"] `shouldReturn` Broken' NaN
    it "Unsaturated/Short" $ do
      test opts ["foo", "-r"]
        `shouldReturn` Failure' (Unsaturated (Short 'r'))
    it "Unsaturated/Long" $ do
      test opts ["foo", "--required"]
        `shouldReturn` Failure' (Unsaturated (Long "required"))
    it "Unsaturated/delimiter" $ do
      test opts ["foo", "-r", "--"] `shouldReturn` Broken' NaN

  describe "Unrecognized" $ do
    it "Short" $ do
      unrecognized <- encodePath "-q"
      test opts ["foo", "-q", "bar"]
        `shouldReturn` Failure' (Unrecognized unrecognized "-q" (Short 'q'))
    it "Short/arg" $ do
      unrecognized <- encodePath "-pquiche"
      test opts ["foo", "-pquiche", "bar"]
        `shouldReturn` Failure' (Unrecognized unrecognized "-pquiche" (Short 'q'))
    it "Long" $ do
      unrecognized <- encodePath "--quiche"
      test opts ["foo", "--quiche", "bar"]
        `shouldReturn` Failure' (Unrecognized unrecognized "--quiche" (Long "quiche"))
    it "Long/arg" $ do
      unrecognized <- encodePath "--quiche=314"
      test opts ["foo", "--quiche=314", "bar"]
        `shouldReturn` Failure' (Unrecognized unrecognized "--quiche=314" (Long "quiche"))

  describe "delimiter" $ do
    it "empty" $ do
      test opts ["foo", "--", "-bar", "baz"] `shouldReturn` Success' 0 ["foo", "-bar", "baz"]

    it "Oversaturated" $ do
      oversaturated <- encodePath "314"
      test opts ["foo", "--=314", "bar"] `shouldReturn` Failure' (Oversaturated "" oversaturated)
