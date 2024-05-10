# claw

A Haskell library for parsing command-line options.


Extremely minimal by design, certain conveniences that come with other libraries
are explicltly denied:

- No implicit state management.
  
  Instead, options invoke arbitrary functions that are applied to the chosen state.
  This may be confusing to set up, see [`claw-example`](/claw-example) for a
  well-structured parser.

- No automatic help document generation.

  The user is expected to write the @--help@ output on their own,
  using whichever tools they deem necessary.
  [`claw-prettyprinter`](/claw-prettyprinter) is a part of this repository as a
  default pretty-printing solution.


On the other hand, this library provides unique features of its own:

- Options may be precompiled using Template Haskell;

- Parsing may be customized however needed by unwrapping `Codec.Console.Options.Decoder`
  manually.
