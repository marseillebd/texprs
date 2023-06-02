# TODO

## Stage 1

- FIXES:
- CLEANUP:
  - [x] use what is now `Reason` as `ReaderError`, rename `ReaderError` to something else
  - [ ] move (de)serializers from Main
  - [ ] better readme
  - [ ] try to remove T.pack/unpack as much as possible
  - [x] newtypes for restricted strings
  - [x] hackage docs
  - [?] module Data.Range (ORDS(DisjointGT,TangentGT,OverlapGT,ContainGT,EQ,..)Ords(compares))
      - possibly just use hackage `range` library
  - [x] output tbnf.tbnf Define form to Haskell
  - [x] import control
  - [x] Rule.Void
  - [x] documentation if TBNF
  - [x] better position type
  - [x] ErrorReport should be a record
  - [x] Texpr.Error (and ErrorReport) should carry a "reason", which is just the expected set + position
- FEATURES:
  - [x] application that parses into texprs
    - `texpr >input <peg-over-strings-file> [<peg-over-texprs-file>...] >output`
    - [x] parse options
    - [x] read and compile grammars
    - [x] output texpr
    - [x] read input texprs
    - [x] output short form texpr
    - [x] render errors nicely
    - [ ] render errors even more nicely
  - [x] allow subtraction in char classes
    - [x] and also, tidy up that syntax pLEASE!
  - [ ] primitive grammars
    - 6 columns: Tree.hs, Monad.hs, Define.hs, Compile.hs, Bootstrap.hs, tbnf.tbnf
    - [x][x][x][x][x][x] shallow Texpr Combo
      - [x] generalize matching algorithm
      - [x] implement matching on texpr lists
    - [x][x][x][x][x][x] Expect
    - [ ][ ][ ][ ][ ][ ] deep Texpr combo
      - '{' lws* Name.ctor (lws* ':' lws+ Rule.Seq)? lws* '}'
    - [ ][ ][ ][ ][ ][ ] deep Texpr atom
      - '{' lws* ':' lws+ Rule lws* '}'
    - [ ][ ][ ][ ][ ][ ] Not
      - takes a message and a rule, fails with message when rule matches
        otherwise backtracks
    - [ ][ ][ ][ ][ ][ ] Intersect
      - returns the parsed value of the first
      - the following ones must match the text the first parsed
    - [ ][ ][ ][ ][ ][ ] Any (char or texpr)
    - [ ][ ][ ][ ][ ][ ] empty sequence
    - [ ][ ][ ][ ][ ][ ] Lookahead
    - [ ][ ][ ][ ][ ][ ] Repeat-Until
  - [ ] simplify/optimize the rules
    - [ ] match many chars in flat mode
    - [ ] un-nest seq and alt
    - [ ] eliminate identity elements for seq and alt
    - [ ] trim away unused rules
  - [ ] extend rules and sets
  - [ ] import grammars
- TESTING:
  - [ ] implement autotab with autotab.tbnf

## Stage 2

- [x] standard for serializing texpr streams
- [ ] texpr rewriter language
  - [ ] define a textual grammar for rewriters
  - [x] the match/rewrite algorithm


## Use Texprs/Trwl

- listdown
- natural deduction unicode -> latex
- autows

## Stage 3+

- [ ] better error detection/reporting for trwl
- [?] when erroring, collect a context along with the input so that parsing can be resumed
- interfaces:
  - [ ] represent grammars (high- and low-level) as json
  - [ ] render grammars to html
