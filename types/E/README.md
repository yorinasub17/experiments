# Playing with Type Theory

- Implementation of toy language E as defined in PFPL


## Goals

- Gain experience learning to implement a compiled language with a type system
- Playground to implement concepts described in various type theory books and lectures:
    - OPLSS
    - PFPL
    - TAPL
    - TTPL

- Keep enhancing toy language with more features in the type system
- Learn how to implement compiler UX, such as sane error messages with suggested fixes and source references


## Core design of language

LLVM based statically compiled language


## Some basic instructions to run code

Require:
`opam install ounit`

Compile with:
`ocamlbuild E.byte`

Execute with:
`./E.byte`
