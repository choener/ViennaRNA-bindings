0.1.4.0

- added circular folding and partition function calculations
- NOTE due to circular dependencies in fold / fold_vars we currently can not
  build on ghc 7.8

0.1.3.0

- added duplexfold
- added a bunch of c/h files due to duplexfold dependencies

0.1.2.1

- removed debug statements

0.1.2.0

- constrained cofold partition function added

0.1.1.1

- export everything in the bindings

0.1.1.0
-------

- breaking changes to PartFunc.chs
- constrained partition function folding
- includes should now all be local (that one was bad)

0.1.0.0
-------

- relevant cbits are now provided as part of the cabal package
