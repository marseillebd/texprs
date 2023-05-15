# TODO

## Stage 1

- FIXES:
- CLEANUP:
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
  - [ ] primitive grammars
    - 6 columns: Tree.hs, Monad.hs, Define.hs, Compile.hs, Bootstrap.hs, tbnf.tbnf
    - [x][x][ ][ ][ ][ ] shallow Texpr Combo
      - [x] generalize matching algorithm
      - [x] implement matching on texpr lists
    - [ ] deep Texpr combo
      - '{' lws* Name.ctor (lws* ':' lws+ Rule.Seq)? lws* '}'
    - [ ] deep Texpr atom
      - '{' lws* ':' lws+ Rule lws* '}'
    - [ ][ ][ ][ ][ ][ ] Not
      - takes a message and a rule, fails with message when rule matches
    - [ ][ ][ ][ ][ ][ ] Any (char or texpr)
    - [ ][ ][ ][ ][ ][ ] Expect
    - [?] Lookahead
    - [?] And (intersection)
  - [ ] simplify/optimize the rules
    - [ ] un-nest seq and alt
    - [ ] eliminate identity elements for seq and alt
  - [ ] extend rules and sets
  - [ ] import grammars
- TESTING:
  - [ ] implement autotab with autotab.tbnf

## Stage 2

- [ ] render grammars to html
- [ ] standard for serializing texpr streams
- [ ] application that parses into texprs
  - `texpr >input <peg-over-strings-file> [<peg-over-texprs-file>...] >output`
- [ ] texpr rewriter language
  - [ ] define a textual grammar for rewriters
  - [ ] the match/rewrite algorithm
- imagine if, instead of just operating over characters, parsers could also operate over texprs
  - then, we could do a lexing phase over characters, followed by a parsing phase over tokens
  - I'd probly use syntax like `[Foo]` to match any texpr with a `Foo` ctor; and that'd be that
    - because if you're using this part of the parser, you don't want to dig inside tokens
    - well, maybe `[Foo: g1 (g2.a g2.b) g3]` to match sub-expressions or `[g1]` to match atoms
- [?] when erroring, collect a context along with the input so that parsing can be resumed
- interfaces:
  - [ ] represent grammars (high- and low-level) as json
