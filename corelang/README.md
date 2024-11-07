# A simple programming language

This language is a continuation of MUPL/Resil language of Coursera PL course.

See [the SML repository](https://github.com/kokoro-aya/coursera-pl/tree/main/mupl-rsl) for more information.

By now, almost all features of the extension assignments have been solved. The work have been ported from
SML to Scala, with several enhancements and bug fixes.

A first version of language implementation includes:

- Basic syntaxes for ADT, tuple, record, list, array, and ref
- Possibility to declare custom data structures
- Simple evaluator
- Simple Hindley-Milner STLC typing system:
  - MUPL code were ported here,
  - Full implementation of unification and constraint resolution
- Augmentation on collections (tuple, record, list, array)

Several assignment exercises remain:

- Pattern matching (ex. 5)
- Explicit typing (ex. 6)
- Multi-param functions (ex. 7)
- Ref and Lazy constructs (ex 8 and 9)