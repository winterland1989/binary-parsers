binary-parsers
==============

This package extends [binary](http://hackage.haskell.org/package/binary) with parsec/attoparsec style parsing combinators.

Haskell's binary package is an excellent choice for bytestring parsing, and the central to haskell's eco-system. Contrary to most misbelief, binary's design allows very fast non-backtracking parsing without book-keeping, while still allows backtracking parsing with little extra overhead. By constructing parsing combinators upon binary, i hope to meet following targets:

+ unify haskell's bytestring parsing toolkit.
+ easier to write `Binary` instances with comprehensive combinators.
+ reduce overall maintaining efforts.

Early benchmark showed that it exceeds attoparsec in almost every case, please join and help!

Benchmarks
----------

Run `cabal bench` to Benchmarks.

```
Benchmark criterion: RUNNING...
benchmarking aeson-attoparsec/buffer-builder
time                 4.269 ms   (4.062 ms .. 4.379 ms)
                     0.966 R²   (0.887 R² .. 0.999 R²)
mean                 4.603 ms   (4.440 ms .. 5.245 ms)
std dev              933.8 μs   (182.1 μs .. 1.937 ms)
variance introduced by outliers: 88% (severely inflated)

benchmarking aeson-attoparsec/dates-fract
time                 4.196 μs   (4.113 μs .. 4.278 μs)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 4.160 μs   (4.111 μs .. 4.209 μs)
std dev              168.2 ns   (141.6 ns .. 202.6 ns)
variance introduced by outliers: 52% (severely inflated)

benchmarking aeson-attoparsec/dates
time                 3.984 μs   (3.923 μs .. 4.085 μs)
                     0.988 R²   (0.974 R² .. 0.998 R²)
mean                 4.128 μs   (3.975 μs .. 4.359 μs)
std dev              610.2 ns   (338.3 ns .. 909.1 ns)
variance introduced by outliers: 94% (severely inflated)

benchmarking aeson-attoparsec/example
time                 75.25 μs   (74.23 μs .. 76.25 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 75.29 μs   (74.70 μs .. 75.87 μs)
std dev              2.027 μs   (1.682 μs .. 2.504 μs)
variance introduced by outliers: 25% (moderately inflated)

benchmarking aeson-attoparsec/geometry
time                 3.371 ms   (3.325 ms .. 3.419 ms)
                     0.998 R²   (0.996 R² .. 0.999 R²)
mean                 3.414 ms   (3.377 ms .. 3.498 ms)
std dev              170.4 μs   (97.13 μs .. 310.1 μs)
variance introduced by outliers: 32% (moderately inflated)

benchmarking aeson-attoparsec/integers
time                 309.6 μs   (301.0 μs .. 325.7 μs)
                     0.985 R²   (0.960 R² .. 0.999 R²)
mean                 308.4 μs   (303.6 μs .. 321.6 μs)
std dev              25.03 μs   (11.12 μs .. 49.08 μs)
variance introduced by outliers: 70% (severely inflated)

benchmarking aeson-attoparsec/jp10
time                 492.4 μs   (486.9 μs .. 498.2 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 495.8 μs   (491.0 μs .. 501.4 μs)
std dev              17.61 μs   (14.58 μs .. 21.21 μs)
variance introduced by outliers: 28% (moderately inflated)

benchmarking aeson-attoparsec/jp100
time                 3.165 ms   (3.075 ms .. 3.275 ms)
                     0.995 R²   (0.992 R² .. 0.999 R²)
mean                 3.107 ms   (3.075 ms .. 3.144 ms)
std dev              109.2 μs   (84.44 μs .. 153.8 μs)
variance introduced by outliers: 19% (moderately inflated)

benchmarking aeson-attoparsec/jp50
time                 1.627 ms   (1.609 ms .. 1.643 ms)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 1.638 ms   (1.621 ms .. 1.661 ms)
std dev              66.69 μs   (51.40 μs .. 100.5 μs)
variance introduced by outliers: 28% (moderately inflated)

benchmarking aeson-attoparsec/numbers
time                 574.5 μs   (562.7 μs .. 585.1 μs)
                     0.997 R²   (0.996 R² .. 0.998 R²)
mean                 566.4 μs   (559.4 μs .. 574.1 μs)
std dev              24.21 μs   (20.78 μs .. 29.16 μs)
variance introduced by outliers: 35% (moderately inflated)

benchmarking aeson-attoparsec/twitter1
time                 25.69 μs   (25.43 μs .. 25.97 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 25.80 μs   (25.52 μs .. 26.14 μs)
std dev              1.066 μs   (863.3 ns .. 1.361 μs)
variance introduced by outliers: 47% (moderately inflated)

benchmarking aeson-attoparsec/twitter10
time                 181.0 μs   (177.8 μs .. 188.2 μs)
                     0.979 R²   (0.932 R² .. 0.999 R²)
mean                 183.0 μs   (178.7 μs .. 202.1 μs)
std dev              25.08 μs   (6.002 μs .. 55.85 μs)
variance introduced by outliers: 89% (severely inflated)

benchmarking aeson-attoparsec/twitter100
time                 2.272 ms   (2.232 ms .. 2.302 ms)
                     0.998 R²   (0.998 R² .. 0.999 R²)
mean                 2.261 ms   (2.238 ms .. 2.284 ms)
std dev              74.10 μs   (60.45 μs .. 98.29 μs)
variance introduced by outliers: 19% (moderately inflated)

benchmarking aeson-attoparsec/twitter20
time                 377.6 μs   (373.6 μs .. 381.6 μs)
                     0.999 R²   (0.999 R² .. 0.999 R²)
mean                 377.1 μs   (374.6 μs .. 381.1 μs)
std dev              10.49 μs   (8.618 μs .. 13.40 μs)
variance introduced by outliers: 20% (moderately inflated)

benchmarking aeson-attoparsec/twitter50
time                 1.110 ms   (1.100 ms .. 1.119 ms)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 1.119 ms   (1.110 ms .. 1.132 ms)
std dev              37.68 μs   (29.28 μs .. 50.49 μs)
variance introduced by outliers: 22% (moderately inflated)

benchmarking aeson-binary-parser/buffer-builder
time                 4.265 ms   (4.212 ms .. 4.315 ms)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 4.322 ms   (4.274 ms .. 4.379 ms)
std dev              167.3 μs   (128.7 μs .. 243.3 μs)
variance introduced by outliers: 20% (moderately inflated)

benchmarking aeson-binary-parser/dates-fract
time                 3.651 μs   (3.541 μs .. 3.803 μs)
                     0.993 R²   (0.986 R² .. 0.999 R²)
mean                 3.615 μs   (3.560 μs .. 3.705 μs)
std dev              228.5 ns   (166.5 ns .. 389.1 ns)
variance introduced by outliers: 73% (severely inflated)

benchmarking aeson-binary-parser/dates
time                 3.501 μs   (3.459 μs .. 3.551 μs)
                     0.989 R²   (0.971 R² .. 0.999 R²)
mean                 3.617 μs   (3.492 μs .. 4.048 μs)
std dev              649.1 ns   (144.2 ns .. 1.374 μs)
variance introduced by outliers: 96% (severely inflated)

benchmarking aeson-binary-parser/example
time                 69.49 μs   (68.03 μs .. 71.10 μs)
                     0.998 R²   (0.996 R² .. 0.999 R²)
mean                 69.05 μs   (68.32 μs .. 69.80 μs)
std dev              2.572 μs   (2.116 μs .. 3.180 μs)
variance introduced by outliers: 39% (moderately inflated)

benchmarking aeson-binary-parser/geometry
time                 2.961 ms   (2.905 ms .. 3.006 ms)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 2.973 ms   (2.946 ms .. 3.008 ms)
std dev              98.87 μs   (78.04 μs .. 136.5 μs)
variance introduced by outliers: 17% (moderately inflated)

benchmarking aeson-binary-parser/integers
time                 221.6 μs   (218.6 μs .. 224.1 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 219.6 μs   (217.3 μs .. 222.1 μs)
std dev              7.507 μs   (6.322 μs .. 9.135 μs)
variance introduced by outliers: 30% (moderately inflated)

benchmarking aeson-binary-parser/jp10
time                 448.6 μs   (445.1 μs .. 452.1 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 450.5 μs   (446.6 μs .. 454.4 μs)
std dev              13.33 μs   (10.97 μs .. 16.23 μs)
variance introduced by outliers: 22% (moderately inflated)

benchmarking aeson-binary-parser/jp100
time                 2.820 ms   (2.772 ms .. 2.863 ms)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 2.825 ms   (2.801 ms .. 2.852 ms)
std dev              87.00 μs   (73.63 μs .. 105.2 μs)
variance introduced by outliers: 15% (moderately inflated)

benchmarking aeson-binary-parser/jp50
time                 1.467 ms   (1.417 ms .. 1.502 ms)
                     0.972 R²   (0.903 R² .. 0.999 R²)
mean                 1.546 ms   (1.466 ms .. 1.939 ms)
std dev              494.8 μs   (45.16 μs .. 1.136 ms)
variance introduced by outliers: 97% (severely inflated)

benchmarking aeson-binary-parser/numbers
time                 427.2 μs   (422.3 μs .. 431.5 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 428.1 μs   (424.0 μs .. 434.3 μs)
std dev              16.60 μs   (11.04 μs .. 29.03 μs)
variance introduced by outliers: 33% (moderately inflated)

benchmarking aeson-binary-parser/twitter1
time                 20.35 μs   (20.11 μs .. 20.57 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 20.22 μs   (19.99 μs .. 20.44 μs)
std dev              742.4 ns   (641.2 ns .. 890.9 ns)
variance introduced by outliers: 43% (moderately inflated)

benchmarking aeson-binary-parser/twitter10
time                 147.6 μs   (144.9 μs .. 151.2 μs)
                     0.997 R²   (0.995 R² .. 0.999 R²)
mean                 147.9 μs   (146.3 μs .. 149.6 μs)
std dev              5.326 μs   (4.251 μs .. 7.077 μs)
variance introduced by outliers: 34% (moderately inflated)

benchmarking aeson-binary-parser/twitter100
time                 2.067 ms   (2.039 ms .. 2.097 ms)
                     0.998 R²   (0.998 R² .. 0.999 R²)
mean                 2.072 ms   (2.055 ms .. 2.091 ms)
std dev              63.41 μs   (53.41 μs .. 77.96 μs)
variance introduced by outliers: 17% (moderately inflated)

benchmarking aeson-binary-parser/twitter20
time                 357.3 μs   (333.3 μs .. 376.8 μs)
                     0.986 R²   (0.982 R² .. 0.998 R²)
mean                 334.6 μs   (328.7 μs .. 343.4 μs)
std dev              22.46 μs   (14.79 μs .. 32.69 μs)
variance introduced by outliers: 61% (severely inflated)

benchmarking aeson-binary-parser/twitter50
time                 964.8 μs   (957.4 μs .. 972.3 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 969.6 μs   (959.8 μs .. 981.7 μs)
std dev              37.25 μs   (27.34 μs .. 58.14 μs)
variance introduced by outliers: 28% (moderately inflated)

Benchmark criterion: FINISH
```
