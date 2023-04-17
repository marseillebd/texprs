# TODO

## Stage 1

- alter star: allow a rule in front which can be matched without requiring the rest to be matched
  - currently, if _anything_ matched under a star, a subsequent error under the same star will be an error
  - however, I'd like to allow some _subset_ of the sequence to match, and _only then_ will subsequent errors lift out of the star
- fixes:
  - [x] report deepest error
    - even if a rule succeeds, it may have had error branches further in
    - an enclosing rule could fail because of an error in a successful rule
    - [x] even on success, save the deepest error
    - [x] when a rule fails after successes, report the deepest between the saved and immediate errors
    - [x] eliminate the repetition-commit part of the grammar
  - [x] if input has not advanced in Star, then the combinator should not recurse
- cleanup:
  - [x] output tbnf.tbnf Define form to Haskell
  - [?] module Data.Range (ORDS(DisjointGT,TangentGT,OverlapGT,ContainGT,EQ,..)Ords(compares))
      - possibly just use hackage `range` library
  - [x] import control
  - [x] Rule.Void
  - [ ] newtypes for restricted strings
    - [ ] global rule names should begin with a capital letter
    - [ ] local rule names and captures should begin with a lowercase letter
    - [ ] instead of inserting `Capture` for rules that have some capitalization, mark it explicitly, but anywhere
          using `<ctor-name> : <grammar>` syntax that binds more loosely than alt
  - [ ] Texpr vs RawTexpr
  - [x] documentation if TBNF
  - [ ] hackage docs
  - [x] better position type
  - [x] ErrorReport should be a record
  - [x] Texpr.Error (and ErrorReport) should carry a "reason", which is just the expected set + position
- [x] test parser: autotab
  - [x] define the grammar in haskell
  - [x] compile texprs into a high-level grammar
  - [x] compiler from a high-level PEG grammar to the core, low-level PEG grammar (`Text.Texpr.Tree`)
    - [x] build graph of char class dependencies; strongly connected components indicate circular definitions
    - [x] eval char classes in reversed breadth-first order
    - [x] xlate the peg rules to tree rules with an environment:
          names in scope, whether we are in flatten
    - [x] test the output of the compiler
    - [ ] simplify/optimize the rules
      - [ ] un-nest seq and alt
      - [ ] patterns for `?` and `+`
  - [ ] test if autotab.tbnf correctly detects tables
- [ ] parser for a language defining PEGs
  - [x] should be literate by default
  - [x] define character sets
  - [ ] define rules
    - [x] define string, char
    - [x] define Sat
    - [x] define Call/Replay
    - [x] define repetition
    - [x] define alternation
    - [ ] define Not; takes a message and a rule, fails with message when rule matches
    - [x] fix Void so it always fails; …or elimiate Void altogether …or require an error message for void
    - [x] define grouping, flatten
    - [ ] define Expect
    - [ ] define Capture
    - [ ] define Recover

## Stage 2

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