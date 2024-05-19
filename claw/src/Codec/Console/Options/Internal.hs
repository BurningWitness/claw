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
import           System.OsString.Custom

import           Data.Foldable (foldlM)
import           System.OsString (OsString)
import qualified System.OsString as Os



-- | Parsing error.
data Failure = -- | Option name isn't on the list.
               Unrecognized
                 !OsString -- ^ The argument that this option is a part of, without
                           --   the preceding dash(es).
                 !Name     -- ^ The option name in question.

               -- | Option requires an argument, but none was provided.
             | Unsaturated
                 !Name

               -- | Option takes no arguments, but was provided with one.
             | Oversaturated
                 !OsString -- ^ Long option name, may be empty (consider @--=ARG@).
                 !OsString -- ^ Argument passed to the option.



-- | Single decoding step, corresponding to processing 1-2 arguments.
data Step f = -- | Parsed a standalone option.
              Single
                f          -- ^ Option itself.
                [OsString] -- ^ Rest of the argument list.

              -- | Parsed a short option that is part of a group.
            | Multiple
                f        -- ^ Option itself.
                (Step f) -- ^ Next option in this group.

              -- | Parsed an argument.
            | Argument
                OsString   -- ^ Argument itself.
                [OsString] -- ^ Rest of the argument list.

              -- | Reached end of options successfully.
            | Done
                [OsString] -- ^ Remaining non-option arguments, if any.

              -- | Could not parse the argument.
            | Failed !Failure

{-# INLINE step #-}
step
  :: (Name -> Maybe (Pair f))
  -> Order
  -> [OsString]
  -> Step f
step lookupName order = go
  where
    go args =
      case args of
        []        -> Done []
        this:rest
          | numWord this >= 2
          , let {  a = Os.index this 0
                ; !b = Os.index this 1

                ; dash = Os.unsafeFromChar '-'
                }

          , a == dash ->
              if b == dash
                then if numWord this == 2
                       then Done rest
                       else
                         case Os.elemIndex (Os.unsafeFromChar '=') this of
                           Nothing -> let !name = Os.drop 2 this
                                      in plainLong name rest

                           Just i  -> let !name = unsafeSlice 2 (i - 2) this
                                          !arg  = Os.drop (i + 1) this

                                      in argLong this name arg rest

                else someShort this 1 b rest

          | otherwise -> argument this rest


    plainLong name args =
      case lookupName (Long name) of
        Nothing   -> Failed $ Unrecognized name (Long name)
        Just pair ->
          case pair of
            Zero  m -> Single m args

            Maybe m -> Single (m Nothing) args

            One   m ->
              case args of
                []        -> Failed $ Unsaturated (Long name)
                next:rest -> Single (m next) rest


    argLong this name arg rest =
      case lookupName (Long name) of
        Nothing   -> let !long = Os.drop 2 this
                     in Failed $ if Os.null name
                                   then Oversaturated name arg
                                   else Unrecognized long (Long name)
        Just pair ->
          case pair of
            Zero  _ -> Failed $ Oversaturated name arg

            Maybe m -> Single (m $ Just arg) rest

            One   m -> Single (m arg) rest


    someShort this i c args =
      let s = Short (Os.toChar c)
      in case lookupName s of
          Nothing   -> let !name = Os.drop 1 this
                       in Failed $ Unrecognized name s
          Just pair ->
            let i' = i + 1
            in case pair of
                 Zero m
                   | i' >= numWord this -> Single m args 
                   | otherwise          ->
                       let !d = Os.index this i'
                       in Multiple m (someShort this i' d args)

                 Maybe m ->
                   let !mayArg | i' >= numWord this = Nothing
                               | otherwise          = Just $! Os.drop i' this

                   in Single (m mayArg) args

                 One m
                   | i' >= numWord this ->
                       case args of
                         []        -> Failed $ Unsaturated s
                         next:rest -> Single (m next) rest

                   | otherwise           ->
                       let !arg = Os.drop i' this
                       in Single (m arg) args


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
newtype Decoder f = Decoder ([OsString] -> Step f)

-- | General result type.
data Result r s = -- | Reached end of options successfully.
                  --
                  --   Returns modified state and the list of non-option arguments
                  --   in the order they were encountered.
                  Success s [OsString]

                  -- | Optional early exit.
                | Break r

                | Failure !Failure

{-# INLINE run #-}
-- | Run the decoder over pure state, sequencing operations left-to-right.
run :: Decoder (s -> Either r s) -> s -> [OsString] -> Result r s
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
runM :: Monad m => Decoder (s -> m (Either r s)) -> s -> [OsString] -> m (Result r s)
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
  :: (OsString -> s -> Either r s) -- ^ Function to apply to every argument.
  -> Decoder (s -> Either r s)
  -> s
  -> [OsString]
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
