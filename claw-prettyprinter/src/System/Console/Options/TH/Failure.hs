{- | t'Codec.Console.Options.Failure' pretty-printing.
 -}

module System.Console.Options.TH.Failure
  ( failure

    -- | === Constituents
  , unrecognized
  , unsaturated
  , oversaturated
  ) where

import           System.Console.Options.Failure.Internal

import           Prettyprinter
import           System.Console.Options.TH
import           System.OsString (OsString)



-- | 'Codec.Console.Options.Unrecognized' failure case, with 'suggestions' if applicable.
--
--   >>> let names = Short 'i' :| [Short 'a', Short 'b', Long "abode"]
--   >>> let opts = $$(precompile $ none .> Option names (plain [|| id ||]))
--   >>> putDocW 80 $ failure opts $ Unrecognized "abid" (Long "abid")
--   invalid option '--abid'; perhaps you meant any of '-a', '-b', '-i' or '--abode'?
unrecognized :: Options Identity f -> OsString -> Name -> Doc ann
unrecognized opts = mkUnrecognized (\arg n -> suggestions arg n opts)



-- | Failure description, with 'suggestions' if applicable.
--
--   Examples for each specific failure case are given below.
failure :: Options Identity f -> Failure -> Doc ann
failure opts f =
  case f of
    Unrecognized arg n -> unrecognized opts arg n
    Unsaturated n      -> unsaturated n
    Oversaturated ls _ -> oversaturated ls
