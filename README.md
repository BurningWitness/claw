# claw

> [!NOTE]
> This branch is a feature-wise duplicate of the `master` one, rewritten to use
> [`os-string#OsString`](https://hackage.haskell.org/package/os-string-2.0.2.2/docs/System-OsString.html#t:OsString)s.
> It only works under GHC 9.10+ (unless GHC is compiled locally) and currently requires
> CPP to get `getArgs` working across platforms (see either of the examples).

Haskell libraries for managing command-line options.

Divided in the following fashion:

- [`claw`](/claw): core types and the parser itself;

- [`claw-prettyprinter`](/claw-prettyprinter): prettyprinter support for
  composing help documents and displaying parsing failures;

- [`claw-example`](/claw-example): example package showcasing the previous two
  together in both runtime and precompiled variations.
