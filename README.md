# Advanced Assembly 0.5.1

Advanced Assembly is a programming language that roughly resembles an assembly programming language. However, unlike standard assembly languages, Advanced Assembly is not designed to be run on a specific processor. Instead, it has been designed so that any series of bits--any combination of "1" and "0"--is a valid program.

Advanced Assembly discards the conventional grouping of eight bits into a byte; instead, bits are divided into groups of arbitrary length. This makes it impossible to run an Advanced Assembly program directly on a processor, but allows instructions to be packed much more densely. Therefore, Advanced Assembly programs must be run in an interpreter. The official Advanced Assembly interpreter, programmed in Haskell, should be included with this document.

During early stages of development, Advanced Assembly was called High Level Binary and  was intended to be able to express a diverse range of paradigms found in high level programming languages. However, as development progressed, it became apparent that such paradigms lead to a complex and convoluted syntax. In order to simplify the syntax, all complex paradigms that could be easily implemented through simpler paradigms were removed.

When the first complete language specification was planned out, High Level Binary no longer represented the "high level" programming languages it was originally intended to emulate; instead, the programming language appeared to be a permutation of a typical assembly language. Therefore, the decision was made to rename the language Advanced Assembly to indicate that it facilitated a different feature set than standard assembly languages while distancing the association with the more advanced high level programming languages.
