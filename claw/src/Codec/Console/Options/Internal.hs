{-# LANGUAGE BangPatterns
           , GADTs #-}

module Codec.Console.Options.Internal
  ( Failure (..)
  , Step (..)
  , step

  , Decoder (..)

  , Order (..)
  , Result (..)
  , run
  , runM
  , runInOrder
  ) where

import           Data.Console.Option.Internal

import           Data.Foldable (foldlM)
import           Data.Text (Text)
import qualified Data.Text as Text



-- | Parsing error.
data Failure = -- | Option name isn't on the list.
               Unrecognized
                 !String -- ^ The argument that this option is a part of, without
                         --   the preceding dash(es).
                 !Name   -- ^ The option name in question.

               -- | Option requires an argument, but none was provided.
             | Unsaturated
                 !Name

               -- | Option takes no arguments, but was provided with one.
             | Oversaturated
                 !Text   -- ^ Long option name, may be empty (consider @--=ARG@).
                 !String -- ^ Argument passed to the option.



-- | Single decoding step, corresponding to processing 1-2 arguments.
data Step f = -- | Parsed a standalone option.
              Single
                f        -- ^ Option itself.
                [String] -- ^ Rest of the argument list.

              -- | Parsed a short option that is part of a group.
            | Multiple
                f        -- ^ Option itself.
                (Step f) -- ^ Next option in this group.

              -- | Parsed an argument.
            | Argument
                String   -- ^ Argument itself.
                [String] -- ^ Rest of the argument list.

              -- | Reached end of options successfully.
            | Done
                [String] -- ^ Remaining non-option arguments, if any.

              -- | Could not parse the argument.
            | Failed !Failure

{-# INLINE step #-}
step
  :: (Name -> Maybe (Pair f))
  -> Order
  -> [String]
  -> Step f
step lookupName order = go
  where
    go args =
      case args of
        []        -> Done []
        this:rest ->
          case this of
            '-':'-':[]   -> Done rest

            '-':'-':long -> let (as, bs) = span (/= '=') long

                            in case bs of
                                 []    -> let !txt = Text.pack long
                                          in plainLong long txt rest

                                 _:arg -> let !txt = Text.pack as
                                          in argLong long txt arg rest

            '-':c:cs     -> someShort (c:cs) c cs rest

            _            -> argument this rest


    plainLong this txt args =
      case lookupName (Long txt) of
        Nothing   -> Failed $ Unrecognized this (Long txt)
        Just pair ->
          case pair of
            Zero  m -> Single m args

            Maybe m -> Single (m Nothing) args

            One   m ->
              case args of
                []        -> Failed $ Unsaturated (Long txt)
                next:rest -> Single (m next) rest


    argLong this txt arg rest =
      case lookupName (Long txt) of
        Nothing   -> Failed $ if Text.null txt
                                then Oversaturated txt arg
                                else Unrecognized this (Long txt)
        Just pair ->
          case pair of
            Zero  _ -> Failed $ Oversaturated txt arg

            Maybe m -> Single (m $ Just arg) rest

            One   m -> Single (m arg) rest


    someShort this c cs args =
      case lookupName (Short c) of
        Nothing   -> Failed $ Unrecognized this (Short c)
        Just pair ->
          case pair of
            Zero  m -> case cs of
                         []   -> Single m args
                         d:ds -> Multiple m (someShort this d ds args)

            Maybe m ->
              let mayArg = case cs of
                             [] -> Nothing
                             _  -> Just cs

              in Single (m mayArg) args

            One   m ->
              case cs of
                []  ->
                  case args of
                    []        -> Failed $ Unsaturated (Short c)
                    next:rest -> Single (m next) rest

                _:_ -> Single (m cs) args


    argument arg rest =
      case order of
        RequireOrder    -> Done (arg:rest)

        Permute         -> Argument arg rest



-- | Decoder operation mode. Similar to "System.Console.GetOpt.ArgOrder".
data Order = -- | First non-option marks the end of options
             RequireOrder

             -- | Non-options are freely interspersed with options
           | Permute

-- | Decoding algorithm without any input applied.
newtype Decoder f = Decoder ([String] -> Step f)

-- | General result type.
data Result r s = -- | Reached end of options successfully.
                  --
                  --   Returns modified state and the list of non-option arguments
                  --   in the order they were encountered.
                  Success s [String]

                  -- | Optional early exit.
                | Break r

                | Failure !Failure

{-# INLINE run #-}
-- | Run the decoder over pure state, sequencing operations left-to-right.
run :: Decoder (s -> Either r s) -> s -> [String] -> Result r s
run (Decoder step_) = float []
  where
    float keep s0 args = go s0 (step_ args)
      where
        go s x =
          case x of
            Single f rest ->
              case f s of
                Right s'      -> float keep s' rest
                Left e        -> Break e

            Multiple f next ->
              case f s of
                Right s'      -> go s' next
                Left e        -> Break e

            Argument arg rest -> float (arg:keep) s rest

            Done xs           -> Success s (rev xs keep)

            Failed e          -> Failure e



{-# INLINE runM #-}
-- | Run the decoder over pure state, sequencing monadic operations left-to-right.
runM :: Monad m => Decoder (s -> m (Either r s)) -> s -> [String] -> m (Result r s)
runM (Decoder step_) = float []
  where
    float keep s0 args = go s0 (step_ args)
      where
        go s x =
          case x of
            Single f rest     -> do
              ei <- f s
              case ei of
                Right s' -> float keep s' rest
                Left e   -> pure $ Break e

            Multiple f next   -> do
              ei <- f s
              case ei of
                Right s' -> go s' next
                Left e   -> pure $ Break e

            Argument arg rest -> float (arg:keep) s rest

            Done xs           -> pure $ Success s (rev xs keep)

            Failed e          -> pure $ Failure e



{-# INLINE runInOrder #-}
-- | Run the decoder over pure state, applying the given function to every argument
--   in place and sequencing operations left-to-right.
runInOrder
  :: (String -> s -> Either r s) -- ^ Function to apply to every argument.
  -> Decoder (s -> Either r s)
  -> s
  -> [String]
  -> Result r s
runInOrder fArg (Decoder step_) = \s args -> go s (step_ args)
  where
    go s x =
      case x of
        Single f rest ->
          case f s of
            Right s'      -> go s' (step_ rest)
            Left e        -> Break e

        Multiple f next ->
          case f s of
            Right s'      -> go s' next
            Left e        -> Break e

        Argument arg rest ->
          case fArg arg s of
            Right s'      -> go s' (step_ rest)
            Left e        -> Break e

        Done xs           ->
          case foldlM (flip fArg) s xs of
            Right s'      -> Success s' []
            Left e        -> Break e

        Failed e          -> Failure e



-- | Reverse the second list and append to the first one.
rev :: [a] -> [a] -> [a]
rev zs xs =
  case xs of
    []   -> zs
    x:ys -> rev (x:zs) ys
