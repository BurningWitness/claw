{-# LANGUAGE GADTs
           , TemplateHaskell #-}

module Data.Console.Option.Internal
  ( Name (..)

  , Flavor (..)
  , Option (..)

  , Function (..)
  , demote
  , demoteQ
  ) where

import           Data.Text.Short (ShortText)
import           Language.Haskell.TH.Syntax hiding (Name)
import           System.OsString (OsString)



-- | Option name.
data Name = Short !Char     -- ^ As in @-s@.
          | Long !ShortText -- ^ As in @--long-option@.



-- | Option behavior.
data Flavor help f arg where
  -- | Option takes no arguments.
  Plain :: Flavor help f f

  -- | Option takes an optional argument.
  Optional
    :: help                                -- ^ Help description for the argument,
                                           -- i.e. ARG in -o, --option[=ARG].
    -> Flavor help f (Maybe OsString -> f)

  -- | Option requires an argument.
  Required
    :: help                                -- ^ Help description for the argument,
                                           -- i.e. ARG in -o, --option[=ARG].
    -> Flavor help f (OsString -> f)



-- | Single option.
data Option help f arg =
       Option
         [Name]              -- ^ Names that can be used to invoke the option.
         (Flavor help f arg)



data Function f where
  Zero  :: f -> Function f
  Maybe :: (Maybe OsString -> f) -> Function f
  One   :: (OsString -> f) -> Function f

demote :: Flavor help f arg -> arg -> Function f
demote flavor f =
  case flavor of
    Plain      -> Zero f
    Optional _ -> Maybe f
    Required _ -> One f

demoteQ :: Flavor help f arg -> Code Q arg -> Code Q (Function f)
demoteQ flavor f =
  case flavor of
    Plain      -> [|| Zero  $$f ||]
    Optional _ -> [|| Maybe $$f ||]
    Required _ -> [|| One   $$f ||]
