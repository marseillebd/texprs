# texprs

[Hackage](https://hackage.haskell.org/package/texprs)
[Github](https://github.com/edemko/texprs)

T-expressions (or t-expr/texpr for short) are a variation on s-expressions
where every combination receives an explicit constructor.
One might be represented like:

```
(lambda "x" (app "f" "x"))
```

In plain language, a t-expr is a rose tree of strings,
    where each non-leaf node is tagged with a "constructor",
    which is just a kebab-case string.

T-exprs are specifically intended for use in representing
    freshly-parsed abstract syntax trees of any grammar.
In particular, every node of a t-expr is tagged with a position from the source text
    to aid in error reporting and syntax highlighting.
Once all textual operations are completed on the source,
    I expect the t-expr to be consumed and transformed into an
    appropriate abstract data type that represents the
    specific grammar that was parsed against.
Until then, t-exprs serve as a useful middle point
    between raw text and fully-abstract syntax.

A major advantage of t-expressions (and also s-expressions) is that parsing is split into two, with the tricky character detection code having already been abstracted away.
The expected workflow is to:

- take source code and run it through a "reader" producing t-exprs
    that conform to the reader's grammar
- transform (a.k.a. "parse") those now-validated t-expressions into an abstract data type:
    perhaps something that can be operated on by
    [nanopass](https://hackage.haskell.org/package/nanopass).

With s-expressions, there is a fixed grammar, which annoying varies somewhat between Lisp dialect.
With t-expressions, the grammar is instead defined by a TBNF definition file.
TBNF grammars are themselves written in tbnf, see [docs/tbnf.tbnf](docs/tbnf.tbnf).
Of course the resulting t-exprs could be interpreted directly,
    but I find that defining a specialized abstract grammar clarifies formal semantics.

Why the name t-expr? What does the @t@ stand for?
Well, it could stand for many things:

- for "tagged-expressions", because of the explicit tagging of combinations,
- or perhaps "text-expressions", as they hold only text
- or perhaps "tree-expressions", as they are meant for nothing more than syntax trees,
    (as opposed to binding "trees" which are really a form of graph),
- or maybe even "token-expression", as it contains
    all the (possibly nested) tokens of a source file,
- or cheekily, because @s++ === t@.

## Contribution Notes

If you change the grammar in `tbnf.tbnf`, then uncomment the `print peg` line in `test/Main.hs` and run `cabal run test`.
This will generate Haskell source code for the grammar that can them be pasted over the contents of `Text.Tbnf.Bootstrap.tbnf`.
