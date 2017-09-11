0.233.2.0
---------

- use ByteString.Char8 instead of String

0.233.1.2

- included C/$ALL.c C/config.h in distribution

0.233.1.1
---------

- track ViennaRNA version numbers in 2nd component
- bump to v 2.3.3

0.1.6.0
-------

- partial bindings to ViennaRNA 2.2.5
- tests/properties.hs

0.1.5.0
-------

- added stack.yaml file

0.1.4.0
-------

- added circular folding and partition function calculations
- NOTE due to circular dependencies in fold / fold_vars we currently can not
  cabal repl on ghc 7.8.{1,2,3}

0.1.3.0
-------

- added duplexfold
- added a bunch of c/h files due to duplexfold dependencies

0.1.2.1
-------

- removed debug statements

0.1.2.0
-------

- constrained cofold partition function added

0.1.1.1
-------

- export everything in the bindings

0.1.1.0
-------

- breaking changes to PartFunc.chs
- constrained partition function folding
- includes should now all be local (that one was bad)

0.1.0.0
-------

- relevant cbits are now provided as part of the cabal package

