## Resil Language implementation

#### Core lang implementation I

Status: DONE

- [x] Syntax:
    - [x] ADT
    - [x] Tuple, Record
    - [x] List, Array, Ref, `:=`
    - [x] Evaluator test
- [x] Custom data structure declaration and resolution I
    - [x] Type declaration lang struct
- [x] Simple typing
    - [x] Port MUPL code here
    - [x] Unification and resolution
    - [x] Testing
    - [x] Augment for simple collections
    - [x] Testing for simple collections (eval + typing)
    - [x] Fix typecheck to infer
    - [x] Testing for collection with complex expressions
    - [x] Generic typing and testing for collections (list, array, record, tuple)

#### Core lang implementation II

Status: DONE

Language augmentation: custom AST datatypes, and pattern matching

- Added supports for custom ADT datatypes
- Added supports for pattern destructuring including:
  - Single variable
  - Simple primitives like list/array, tuples and records
  - Custom ADT
  - Typed variable
  - And nested patterns
- Fixed several old typing and evaluation bugs including the losing of constraints
- And introduced several new bugs, also identified several bugs