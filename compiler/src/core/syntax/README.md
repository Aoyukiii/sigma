# `syntax` module

This is the most important module, containing kinds of ASTs, implementation of lexer, parser, and will be more.

One possible architecture:

```
raw AST ---> elaborated AST (---> desugared AST) ---> core AST ---> G-instructions ---> expression graph
                        |                   |        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                        +-------------------+                      G-machine-based backend
                 (Still not sure about the order here)
```