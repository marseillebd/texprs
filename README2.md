USE THIS TO REWRITE THE README

provides
  - texprs: a data type representing parse trees
  - tbnf: a language for defining grammars taking {strings,texpr streams} to texprs
  - trwl: a language for defining rewrite passes over texprs

specifically built for:
  - excellent error reporting in case of a parse/rewrite error,
    because humans need to understand where/how the input is malformed and fix it
  - _not_ for speed, unless it becomes a bottleneck somehow.
    I expect compilers to 
  - tbnf grammars do not contain non-comparable/non-serializable components.
    this makes it possible, in theory, to analyze a grammar, and perhaps compile it to native code.
    For now, we have tbnf is implemented as an interpreter.

alternate solutions:
  - custom (usually internal) data types, custom parsers/lexers, custom rewriters
    - that's a lotta code!
  - s-expressions
    - many variants
    - do not naturally store(serialize/deserialize) source location
  - json, xml, and their ilk
    - does not naturally store source location

notice that:
  - texprs are _parse_ trees, _not_ syntax trees;
    they store information about a single file/input stream,
    _not_ when syntax trees are put together (through inlining, macro expansion, and so on)
  - once you have the source parse, you will likely want to transform it into an abstract data type
      representing the abstract syntax tree of your language.
    However, all of the tricky character manipulation is done, so it should be as easy as traversing your texpr stream.
    What this really means is that the process of turning source code into AST is now broken in two:
      "reading" from text to texprs, then "parsing" from texprs to your language's AST.
    This is much like how one can read JSON data from text,
      but then one must recognize your data type within the resulting mess of JSON.
    This separation also occurs in Lisp, and is extremely convenient.
    Notice however, that while Lisp has a (scare quotes) fixed grammar
      that can be programmatically (threaded with reading) extended with reader macros,
      whereas this system can work with many grammars, but cannot programmatically extend them.
    This is by design, as reader macros are... not great IMHO.
      (Sure, they can do a lot, but also they can make the language unrecognizable on a whim.)