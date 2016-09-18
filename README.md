binary-parsers
==============

This package extends [binary](http://hackage.haskell.org/package/binary) with parsec/attoparsec style parsing combinators.

Haskell's binary package is an excellent choice for bytestring parsing, and the central to haskell's eco-system. Contrary to most misbelief, binary's design allows very fast non-backtracking parsing without book-keeping, while still allows backtracking parsing with some extra overhead. By constructing parsing combinators upon binary, i hope to meet following targets:

+ unify haskell's bytestring parsing toolkit.
+ easier to write `Binary` instances with comprehensive combinators.
+ reduce overall maintaining efforts.

Early benchmark showed that it can even exceed attoparsec under some circumstances, please join and help!
