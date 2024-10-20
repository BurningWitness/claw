# claw

> [!CAUTION]
> This branch blindly assumes that command-line arguments on POSIX-compliant platforms
> are passed as UTF-8. This assumption is in fact incorrect.
> However, the overwhelming majority of used encodings extend ASCII, so
> it's *generally* safe to use this branch if all declared options are ASCII-compatible.
>
> Unfortunately making this library POSIX-compliant is at the current moment
> near-impossible.  `os-string` provides neither the types to convert to, nor the
> direct functions for efficiently encoding to and from Unicode. Handrolling all of this
> functionality inside this package would make it unnecessarily brittle and would require
> rigorous testing, so I choose to keep it in limbo for the time being.

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
