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
- Basic custom ADT datatypes
- Pattern matching, with possibility to type explicitly

- Currently, multi-param functions can be achieved by tuples

Several assignment exercises remain:

- Ref and Lazy constructs (ex 8 and 9)

# Note

Please use JDK 11 to compile this project:
- From `Project Structure > SDK`
- And `Settings > Build, Execution, Deployment > Build Tools > sbt > General Settings > JRE`.