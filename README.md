# origami

Origami is an un-SYB Haskell framework for transforming heterogenous
data through folds.  It consists of a coding pattern for transforming
heterogenously typed data and Template Haskell macros to support the
pattern.  It operates in the same problem space as SYB, but generates
the boilerplate for the user.

In the pattern, we focus on a specific set of types, the *fold
family*.  For each constructor of each of those types, the user will
define a function to "replace" the constructor when folding.

These functions are collected together into a single record.  (Its
declaration is generated for the user.)  Collecting the functions
together forces the typing of all the functions to remain
consistent and facilitates code reuse.

For each datatype, a fold function is automatically generated that
applies the fold record to values of that datatype.  It decomposes
the value into parts according to its constructor, recursively
applies the fold to the parts, then puts them back together using
the replacement constructor function from the record.

As with SYB, Origami lets the user think on a higher-level of
abstraction about data transformation and relieves her from the
need to write boilerplate.

Code examples will be added soon.
