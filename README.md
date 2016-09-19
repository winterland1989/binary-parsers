binary-parsers
==============

This package extends [binary](http://hackage.haskell.org/package/binary) with parsec/attoparsec style parsing combinators.

Haskell's binary package is an excellent choice for bytestring parsing, and the central to haskell's eco-system. Contrary to most misbelief, binary's design allows very fast non-backtracking parsing without book-keeping, while still allows backtracking parsing with little extra overhead. By constructing parsing combinators upon binary, i hope to meet following targets:

+ unify haskell's bytestring parsing toolkit.
+ easier to write `Binary` instances with comprehensive combinators.
+ reduce overall maintaining efforts.

Currently this library passed attoparsec's test suit, and JSON parsing unit tests. 

Benchmarks
----------

Run `cabal bench` to Benchmarks, it exceeds attoparsec in almost every case : )

```
Linking dist/build/criterion/criterion ...
Running 1 benchmarks...
Benchmark criterion: RUNNING...
benchmarking aeson-attoparsec/buffer-builder
time                 4.385 ms   (4.233 ms .. 4.603 ms)
                     0.991 R²   (0.981 R² .. 0.999 R²)
mean                 4.372 ms   (4.324 ms .. 4.445 ms)
std dev              194.0 μs   (132.3 μs .. 316.0 μs)
variance introduced by outliers: 24% (moderately inflated)

benchmarking aeson-attoparsec/dates-fract
time                 4.121 μs   (4.075 μs .. 4.167 μs)
                     0.999 R²   (0.999 R² .. 0.999 R²)
mean                 4.139 μs   (4.097 μs .. 4.188 μs)
std dev              155.7 ns   (121.2 ns .. 207.5 ns)
variance introduced by outliers: 48% (moderately inflated)

benchmarking aeson-attoparsec/dates
time                 3.933 μs   (3.901 μs .. 3.966 μs)
                     0.999 R²   (0.999 R² .. 0.999 R²)
mean                 3.921 μs   (3.877 μs .. 3.960 μs)
std dev              134.5 ns   (111.8 ns .. 159.6 ns)
variance introduced by outliers: 44% (moderately inflated)

benchmarking aeson-attoparsec/example
time                 74.25 μs   (73.41 μs .. 75.42 μs)
                     0.968 R²   (0.900 R² .. 0.999 R²)
mean                 79.00 μs   (74.40 μs .. 99.71 μs)
std dev              27.77 μs   (2.503 μs .. 63.50 μs)
variance introduced by outliers: 99% (severely inflated)

benchmarking aeson-attoparsec/geometry
time                 3.302 ms   (3.243 ms .. 3.346 ms)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 3.293 ms   (3.262 ms .. 3.326 ms)
std dev              106.8 μs   (88.60 μs .. 134.4 μs)
variance introduced by outliers: 16% (moderately inflated)

benchmarking aeson-attoparsec/integers
time                 302.9 μs   (299.6 μs .. 306.4 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 304.3 μs   (301.5 μs .. 307.3 μs)
std dev              9.928 μs   (7.995 μs .. 12.73 μs)
variance introduced by outliers: 27% (moderately inflated)

benchmarking aeson-attoparsec/jp10
time                 471.1 μs   (466.9 μs .. 475.2 μs)
                     0.999 R²   (0.999 R² .. 0.999 R²)
mean                 471.4 μs   (467.1 μs .. 474.8 μs)
std dev              13.31 μs   (10.83 μs .. 16.46 μs)
variance introduced by outliers: 20% (moderately inflated)

benchmarking aeson-attoparsec/jp100
time                 2.963 ms   (2.925 ms .. 3.002 ms)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 3.001 ms   (2.973 ms .. 3.032 ms)
std dev              102.2 μs   (82.67 μs .. 124.1 μs)
variance introduced by outliers: 19% (moderately inflated)

benchmarking aeson-attoparsec/jp50
time                 1.558 ms   (1.540 ms .. 1.578 ms)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 1.574 ms   (1.557 ms .. 1.595 ms)
std dev              62.95 μs   (52.99 μs .. 75.10 μs)
variance introduced by outliers: 28% (moderately inflated)

benchmarking aeson-attoparsec/numbers
time                 551.4 μs   (543.1 μs .. 561.0 μs)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 552.2 μs   (547.0 μs .. 558.5 μs)
std dev              19.13 μs   (15.97 μs .. 26.17 μs)
variance introduced by outliers: 27% (moderately inflated)

benchmarking aeson-attoparsec/twitter1
time                 24.55 μs   (23.87 μs .. 25.26 μs)
                     0.996 R²   (0.994 R² .. 0.999 R²)
mean                 24.10 μs   (23.77 μs .. 24.46 μs)
std dev              1.147 μs   (940.0 ns .. 1.506 μs)
variance introduced by outliers: 55% (severely inflated)

benchmarking aeson-attoparsec/twitter10
time                 169.0 μs   (167.4 μs .. 170.7 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 169.0 μs   (167.4 μs .. 171.2 μs)
std dev              6.270 μs   (4.901 μs .. 8.706 μs)
variance introduced by outliers: 35% (moderately inflated)

benchmarking aeson-attoparsec/twitter100
time                 2.157 ms   (2.112 ms .. 2.203 ms)
                     0.979 R²   (0.953 R² .. 0.995 R²)
mean                 2.263 ms   (2.194 ms .. 2.417 ms)
std dev              303.3 μs   (173.1 μs .. 511.8 μs)
variance introduced by outliers: 79% (severely inflated)

benchmarking aeson-attoparsec/twitter20
time                 357.7 μs   (354.4 μs .. 361.1 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 357.2 μs   (354.1 μs .. 360.2 μs)
std dev              10.16 μs   (8.581 μs .. 12.17 μs)
variance introduced by outliers: 21% (moderately inflated)

benchmarking aeson-attoparsec/twitter50
time                 1.131 ms   (1.052 ms .. 1.267 ms)
                     0.963 R²   (0.931 R² .. 0.998 R²)
mean                 1.078 ms   (1.060 ms .. 1.137 ms)
std dev              93.89 μs   (37.62 μs .. 200.4 μs)
variance introduced by outliers: 67% (severely inflated)

benchmarking aeson-binary-parser/buffer-builder
time                 3.788 ms   (3.723 ms .. 3.852 ms)
                     0.998 R²   (0.996 R² .. 0.999 R²)
mean                 3.928 ms   (3.865 ms .. 4.156 ms)
std dev              356.2 μs   (93.96 μs .. 726.2 μs)
variance introduced by outliers: 58% (severely inflated)

benchmarking aeson-binary-parser/dates-fract
time                 3.228 μs   (3.191 μs .. 3.266 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 3.234 μs   (3.205 μs .. 3.268 μs)
std dev              109.6 ns   (86.59 ns .. 153.7 ns)
variance introduced by outliers: 44% (moderately inflated)

benchmarking aeson-binary-parser/dates
time                 3.132 μs   (3.107 μs .. 3.159 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 3.139 μs   (3.117 μs .. 3.168 μs)
std dev              86.28 ns   (70.07 ns .. 110.5 ns)
variance introduced by outliers: 34% (moderately inflated)

benchmarking aeson-binary-parser/example
time                 57.65 μs   (57.09 μs .. 58.24 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 57.90 μs   (57.29 μs .. 58.48 μs)
std dev              1.918 μs   (1.534 μs .. 2.595 μs)
variance introduced by outliers: 34% (moderately inflated)

benchmarking aeson-binary-parser/geometry
time                 2.744 ms   (2.598 ms .. 2.995 ms)
                     0.968 R²   (0.928 R² .. 0.998 R²)
mean                 2.639 ms   (2.590 ms .. 2.747 ms)
std dev              234.7 μs   (96.30 μs .. 417.6 μs)
variance introduced by outliers: 62% (severely inflated)

benchmarking aeson-binary-parser/integers
time                 180.6 μs   (172.3 μs .. 192.7 μs)
                     0.979 R²   (0.953 R² .. 0.999 R²)
mean                 177.3 μs   (173.9 μs .. 185.1 μs)
std dev              16.16 μs   (7.224 μs .. 32.84 μs)
variance introduced by outliers: 77% (severely inflated)

benchmarking aeson-binary-parser/jp10
time                 488.9 μs   (483.8 μs .. 494.0 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 494.8 μs   (489.8 μs .. 501.6 μs)
std dev              19.89 μs   (15.44 μs .. 25.50 μs)
variance introduced by outliers: 34% (moderately inflated)

benchmarking aeson-binary-parser/jp100
time                 2.694 ms   (2.661 ms .. 2.727 ms)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 2.717 ms   (2.695 ms .. 2.746 ms)
std dev              82.44 μs   (67.32 μs .. 102.4 μs)
variance introduced by outliers: 16% (moderately inflated)

benchmarking aeson-binary-parser/jp50
time                 1.504 ms   (1.487 ms .. 1.521 ms)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 1.512 ms   (1.501 ms .. 1.526 ms)
std dev              40.19 μs   (33.60 μs .. 49.75 μs)
variance introduced by outliers: 15% (moderately inflated)

benchmarking aeson-binary-parser/numbers
time                 357.3 μs   (353.8 μs .. 361.1 μs)
                     0.999 R²   (0.999 R² .. 0.999 R²)
mean                 361.0 μs   (358.0 μs .. 365.3 μs)
std dev              12.11 μs   (9.308 μs .. 17.92 μs)
variance introduced by outliers: 27% (moderately inflated)

benchmarking aeson-binary-parser/twitter1
time                 17.87 μs   (17.75 μs .. 18.01 μs)
                     0.999 R²   (0.999 R² .. 0.999 R²)
mean                 18.05 μs   (17.89 μs .. 18.21 μs)
std dev              573.5 ns   (472.3 ns .. 774.4 ns)
variance introduced by outliers: 36% (moderately inflated)

benchmarking aeson-binary-parser/twitter10
time                 132.2 μs   (127.8 μs .. 140.5 μs)
                     0.959 R²   (0.885 R² .. 0.999 R²)
mean                 131.9 μs   (128.0 μs .. 146.7 μs)
std dev              23.40 μs   (3.675 μs .. 48.96 μs)
variance introduced by outliers: 93% (severely inflated)

benchmarking aeson-binary-parser/twitter100
time                 1.880 ms   (1.860 ms .. 1.896 ms)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 1.888 ms   (1.872 ms .. 1.907 ms)
std dev              56.28 μs   (45.54 μs .. 69.06 μs)
variance introduced by outliers: 16% (moderately inflated)

benchmarking aeson-binary-parser/twitter20
time                 309.7 μs   (306.7 μs .. 312.6 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 310.1 μs   (307.3 μs .. 313.1 μs)
std dev              10.51 μs   (7.815 μs .. 16.03 μs)
variance introduced by outliers: 29% (moderately inflated)

benchmarking aeson-binary-parser/twitter50
time                 913.8 μs   (905.8 μs .. 921.1 μs)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 909.9 μs   (901.3 μs .. 916.6 μs)
std dev              24.88 μs   (19.44 μs .. 32.34 μs)
variance introduced by outliers: 17% (moderately inflated)

Benchmark criterion: FINISH
```
