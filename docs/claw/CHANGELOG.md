## 0.2.0.0 -- August 2025

* Rewritten to properly support `OsString`s.

* `Codec.Console.Options` now only deals with decoding any single command-line argument.

* Moved all option tree handling from `System.Console.Options` and
    `System.Console.Options.TH` to `Data.Console.Options`.

* Removed `Data.Console.Option`, relevant definitions are
    in `Data.Console.Options` instead.

* Removed `System.Console.Options.TH`, option tree precompilation is now handled via
   `Data.Console.Options.insertQ` and ``Data.Console.Options.sequenceCode`.

* `System.Console.Options` now only deals with parsing, only supports the `Permute`
  mode, and runs in `IO` over mutable state to facilitate `OsString` conversions in place.
  Also no longer re-exports `Data.Functor.Identity`.


## 0.1.0.0 -- May 2024

* Initial release
