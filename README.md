binary-parsers
==============

[![Hackage](https://img.shields.io/hackage/v/binary-parsers.svg?style=flat)](http://hackage.haskell.org/package/binary-parsers)
[![Build Status](https://travis-ci.org/winterland1989/binary-parsers.svg)](https://travis-ci.org/winterland1989/binary-parsers)

This package extends [binary](http://hackage.haskell.org/package/binary) with parsec/attoparsec style parsing combinators. It's useful when you want to deal with various binary format, and it's very fast. You can now write more complex `Binary` instances using comprehensive combinators, with serialisation packages like blaze-texual.

Binary's `Get` monad is designed to perform best on non-backtracking cases, but it still provides fast backtracking support via `Alternative` instance, it's overall an excellent alternative to attoparsec if you only deal with `ByteString`.

Building
--------

binary-parsers comes with a test suite modified from attoparsec, and a JSON parsing benchmarks. Here you go:

```
git clone https://github.com/winterland1989/binary-parsers.git
cd binary-parsers
cabal update
cabal configure --enable-tests --enable-benchmarks
cabal build
```

Run the test suite and benchmarks.

```
cabal test
cabal bench
```

Benchmark
---------

```
start benchmark http request parser
benchmarking http-req/attoparsec
time                 2.339 μs   (2.321 μs .. 2.358 μs)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 2.332 μs   (2.307 μs .. 2.365 μs)
std dev              93.73 ns   (65.36 ns .. 147.2 ns)
variance introduced by outliers: 54% (severely inflated)

benchmarking http-req/binary-parsers
time                 1.387 μs   (1.373 μs .. 1.400 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 1.393 μs   (1.378 μs .. 1.412 μs)
std dev              56.09 ns   (43.06 ns .. 77.02 ns)
variance introduced by outliers: 55% (severely inflated)

benchmarking http-req/scanner
time                 1.553 μs   (1.407 μs .. 1.817 μs)
                     0.923 R²   (0.873 R² .. 0.999 R²)
mean                 1.457 μs   (1.416 μs .. 1.647 μs)
std dev              232.3 ns   (41.02 ns .. 518.5 ns)
variance introduced by outliers: 95% (severely inflated)

benchmarking http-req/warp
time                 939.2 ns   (929.0 ns .. 950.5 ns)
                     0.999 R²   (0.997 R² .. 0.999 R²)
mean                 955.2 ns   (943.9 ns .. 973.0 ns)
std dev              48.11 ns   (36.60 ns .. 62.44 ns)
variance introduced by outliers: 67% (severely inflated)

start benchmark JSON parser
benchmarking attoparsec/buffer-builder
time                 4.306 ms   (4.222 ms .. 4.382 ms)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 4.334 ms   (4.286 ms .. 4.382 ms)
std dev              143.9 μs   (117.8 μs .. 184.0 μs)
variance introduced by outliers: 15% (moderately inflated)

benchmarking binary-parser/buffer-builder
time                 3.751 ms   (3.678 ms .. 3.830 ms)
                     0.997 R²   (0.995 R² .. 0.999 R²)
mean                 3.802 ms   (3.770 ms .. 3.845 ms)
std dev              120.5 μs   (97.00 μs .. 153.3 μs)
variance introduced by outliers: 14% (moderately inflated)

benchmarking attoparsec/lazy-bytestring/buffer-builder
time                 4.407 ms   (4.324 ms .. 4.473 ms)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 4.386 ms   (4.349 ms .. 4.425 ms)
std dev              123.3 μs   (102.2 μs .. 156.8 μs)
variance introduced by outliers: 11% (moderately inflated)

benchmarking binary-parser/lazy-bytestring/buffer-builder
time                 3.686 ms   (3.631 ms .. 3.734 ms)
                     0.998 R²   (0.998 R² .. 0.999 R²)
mean                 3.705 ms   (3.676 ms .. 3.736 ms)
std dev              99.87 μs   (84.11 μs .. 122.9 μs)
variance introduced by outliers: 12% (moderately inflated)

benchmarking attoparsec/dates-fract
time                 4.391 μs   (4.211 μs .. 4.727 μs)
                     0.939 R²   (0.847 R² .. 0.999 R²)
mean                 4.420 μs   (4.225 μs .. 5.433 μs)
std dev              1.113 μs   (162.8 ns .. 2.656 μs)
variance introduced by outliers: 98% (severely inflated)

benchmarking binary-parser/dates-fract
time                 3.180 μs   (3.161 μs .. 3.198 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 3.180 μs   (3.159 μs .. 3.207 μs)
std dev              81.49 ns   (69.06 ns .. 97.57 ns)
variance introduced by outliers: 31% (moderately inflated)

benchmarking attoparsec/lazy-bytestring/dates-fract
time                 4.283 μs   (4.224 μs .. 4.344 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 4.240 μs   (4.196 μs .. 4.283 μs)
std dev              149.1 ns   (122.7 ns .. 178.2 ns)
variance introduced by outliers: 45% (moderately inflated)

benchmarking binary-parser/lazy-bytestring/dates-fract
time                 3.241 μs   (3.207 μs .. 3.273 μs)
                     0.999 R²   (0.997 R² .. 0.999 R²)
mean                 3.247 μs   (3.218 μs .. 3.301 μs)
std dev              128.4 ns   (94.42 ns .. 199.9 ns)
variance introduced by outliers: 52% (severely inflated)

benchmarking attoparsec/dates
time                 4.000 μs   (3.967 μs .. 4.033 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 4.025 μs   (3.987 μs .. 4.075 μs)
std dev              141.9 ns   (109.4 ns .. 221.2 ns)
variance introduced by outliers: 45% (moderately inflated)

benchmarking binary-parser/dates
time                 3.049 μs   (3.021 μs .. 3.077 μs)
                     0.996 R²   (0.988 R² .. 0.999 R²)
mean                 3.132 μs   (3.049 μs .. 3.379 μs)
std dev              477.9 ns   (103.8 ns .. 910.2 ns)
variance introduced by outliers: 94% (severely inflated)

benchmarking attoparsec/lazy-bytestring/dates
time                 4.096 μs   (4.020 μs .. 4.243 μs)
                     0.930 R²   (0.807 R² .. 0.999 R²)
mean                 4.490 μs   (4.116 μs .. 5.966 μs)
std dev              2.110 μs   (240.5 ns .. 4.653 μs)
variance introduced by outliers: 99% (severely inflated)

benchmarking binary-parser/lazy-bytestring/dates
time                 3.136 μs   (3.110 μs .. 3.157 μs)
                     0.999 R²   (0.999 R² .. 0.999 R²)
mean                 3.132 μs   (3.103 μs .. 3.180 μs)
std dev              126.2 ns   (93.20 ns .. 177.0 ns)
variance introduced by outliers: 53% (severely inflated)

benchmarking attoparsec/example
time                 76.28 μs   (75.38 μs .. 77.20 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 75.69 μs   (74.99 μs .. 76.51 μs)
std dev              2.453 μs   (2.093 μs .. 3.071 μs)
variance introduced by outliers: 32% (moderately inflated)

benchmarking binary-parser/example
time                 59.48 μs   (58.77 μs .. 60.25 μs)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 60.26 μs   (59.42 μs .. 61.33 μs)
std dev              3.214 μs   (2.459 μs .. 4.060 μs)
variance introduced by outliers: 58% (severely inflated)

benchmarking attoparsec/lazy-bytestring/example
time                 78.84 μs   (77.75 μs .. 80.17 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 78.02 μs   (77.28 μs .. 78.89 μs)
std dev              2.683 μs   (2.172 μs .. 3.398 μs)
variance introduced by outliers: 35% (moderately inflated)

benchmarking binary-parser/lazy-bytestring/example
time                 59.20 μs   (58.64 μs .. 59.79 μs)
                     0.999 R²   (0.997 R² .. 0.999 R²)
mean                 59.23 μs   (58.52 μs .. 60.08 μs)
std dev              2.558 μs   (1.963 μs .. 3.353 μs)
variance introduced by outliers: 47% (moderately inflated)

benchmarking attoparsec/geometry
time                 3.366 ms   (3.323 ms .. 3.416 ms)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 3.427 ms   (3.398 ms .. 3.466 ms)
std dev              107.4 μs   (80.60 μs .. 162.5 μs)
variance introduced by outliers: 16% (moderately inflated)

benchmarking binary-parser/geometry
time                 2.604 ms   (2.572 ms .. 2.635 ms)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 2.619 ms   (2.595 ms .. 2.645 ms)
std dev              79.33 μs   (63.44 μs .. 102.1 μs)
variance introduced by outliers: 15% (moderately inflated)

benchmarking attoparsec/lazy-bytestring/geometry
time                 3.492 ms   (3.420 ms .. 3.565 ms)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 3.440 ms   (3.411 ms .. 3.468 ms)
std dev              96.20 μs   (82.36 μs .. 114.1 μs)
variance introduced by outliers: 12% (moderately inflated)

benchmarking binary-parser/lazy-bytestring/geometry
time                 2.670 ms   (2.633 ms .. 2.698 ms)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 2.647 ms   (2.623 ms .. 2.674 ms)
std dev              84.02 μs   (68.69 μs .. 104.0 μs)
variance introduced by outliers: 16% (moderately inflated)

benchmarking attoparsec/integers
time                 304.8 μs   (301.6 μs .. 308.5 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 306.9 μs   (304.2 μs .. 311.4 μs)
std dev              11.48 μs   (8.233 μs .. 19.62 μs)
variance introduced by outliers: 33% (moderately inflated)

benchmarking binary-parser/integers
time                 212.2 μs   (200.8 μs .. 235.2 μs)
                     0.971 R²   (0.940 R² .. 0.999 R²)
mean                 206.9 μs   (203.5 μs .. 218.2 μs)
std dev              18.14 μs   (6.445 μs .. 39.25 μs)
variance introduced by outliers: 75% (severely inflated)

benchmarking attoparsec/lazy-bytestring/integers
time                 311.6 μs   (308.6 μs .. 316.6 μs)
                     0.998 R²   (0.995 R² .. 0.999 R²)
mean                 311.2 μs   (307.7 μs .. 315.7 μs)
std dev              13.30 μs   (9.673 μs .. 17.54 μs)
variance introduced by outliers: 39% (moderately inflated)

benchmarking binary-parser/lazy-bytestring/integers
time                 201.3 μs   (198.6 μs .. 203.8 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 203.0 μs   (201.3 μs .. 204.8 μs)
std dev              5.859 μs   (4.853 μs .. 7.421 μs)
variance introduced by outliers: 24% (moderately inflated)

benchmarking attoparsec/jp10
time                 500.6 μs   (489.3 μs .. 513.9 μs)
                     0.997 R²   (0.996 R² .. 0.998 R²)
mean                 498.7 μs   (493.5 μs .. 504.2 μs)
std dev              18.67 μs   (15.01 μs .. 23.11 μs)
variance introduced by outliers: 31% (moderately inflated)

benchmarking binary-parser/jp10
time                 467.6 μs   (462.5 μs .. 473.7 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 470.5 μs   (466.4 μs .. 475.8 μs)
std dev              15.54 μs   (12.46 μs .. 19.79 μs)
variance introduced by outliers: 26% (moderately inflated)

benchmarking attoparsec/lazy-bytestring/jp10
time                 491.5 μs   (485.0 μs .. 497.6 μs)
                     0.998 R²   (0.998 R² .. 0.999 R²)
mean                 493.7 μs   (488.3 μs .. 500.1 μs)
std dev              19.88 μs   (15.55 μs .. 29.45 μs)
variance introduced by outliers: 34% (moderately inflated)

benchmarking binary-parser/lazy-bytestring/jp10
time                 499.3 μs   (461.5 μs .. 590.2 μs)
                     0.856 R²   (0.723 R² .. 0.999 R²)
mean                 486.9 μs   (467.1 μs .. 565.2 μs)
std dev              120.4 μs   (12.86 μs .. 252.7 μs)
variance introduced by outliers: 96% (severely inflated)

benchmarking attoparsec/jp100
time                 3.023 ms   (2.989 ms .. 3.059 ms)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 3.079 ms   (3.045 ms .. 3.120 ms)
std dev              122.8 μs   (88.92 μs .. 187.5 μs)
variance introduced by outliers: 23% (moderately inflated)

benchmarking binary-parser/jp100
time                 2.795 ms   (2.762 ms .. 2.828 ms)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 2.790 ms   (2.765 ms .. 2.816 ms)
std dev              83.75 μs   (65.15 μs .. 108.6 μs)
variance introduced by outliers: 15% (moderately inflated)

benchmarking attoparsec/lazy-bytestring/jp100
time                 3.145 ms   (3.094 ms .. 3.199 ms)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 3.117 ms   (3.089 ms .. 3.147 ms)
std dev              94.41 μs   (77.22 μs .. 119.1 μs)
variance introduced by outliers: 15% (moderately inflated)

benchmarking binary-parser/lazy-bytestring/jp100
time                 2.761 ms   (2.724 ms .. 2.798 ms)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 2.756 ms   (2.735 ms .. 2.782 ms)
std dev              70.93 μs   (57.46 μs .. 94.99 μs)
variance introduced by outliers: 11% (moderately inflated)

benchmarking attoparsec/jp50
time                 1.634 ms   (1.617 ms .. 1.654 ms)
                     0.997 R²   (0.993 R² .. 0.999 R²)
mean                 1.629 ms   (1.608 ms .. 1.659 ms)
std dev              82.30 μs   (55.08 μs .. 123.1 μs)
variance introduced by outliers: 36% (moderately inflated)

benchmarking binary-parser/jp50
time                 1.552 ms   (1.464 ms .. 1.739 ms)
                     0.879 R²   (0.730 R² .. 0.999 R²)
mean                 1.509 ms   (1.449 ms .. 1.742 ms)
std dev              344.4 μs   (52.96 μs .. 710.0 μs)
variance introduced by outliers: 93% (severely inflated)

benchmarking attoparsec/lazy-bytestring/jp50
time                 1.668 ms   (1.649 ms .. 1.692 ms)
                     0.998 R²   (0.998 R² .. 0.999 R²)
mean                 1.693 ms   (1.678 ms .. 1.713 ms)
std dev              59.58 μs   (49.87 μs .. 72.55 μs)
variance introduced by outliers: 22% (moderately inflated)

benchmarking binary-parser/lazy-bytestring/jp50
time                 1.471 ms   (1.449 ms .. 1.497 ms)
                     0.998 R²   (0.996 R² .. 0.999 R²)
mean                 1.471 ms   (1.455 ms .. 1.494 ms)
std dev              64.25 μs   (47.02 μs .. 95.22 μs)
variance introduced by outliers: 30% (moderately inflated)

benchmarking attoparsec/numbers
time                 563.8 μs   (557.0 μs .. 569.9 μs)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 562.5 μs   (556.8 μs .. 569.2 μs)
std dev              20.30 μs   (15.46 μs .. 31.17 μs)
variance introduced by outliers: 28% (moderately inflated)

benchmarking binary-parser/numbers
time                 376.8 μs   (373.2 μs .. 380.7 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 380.4 μs   (377.2 μs .. 383.7 μs)
std dev              10.72 μs   (8.971 μs .. 14.59 μs)
variance introduced by outliers: 21% (moderately inflated)

benchmarking attoparsec/lazy-bytestring/numbers
time                 559.7 μs   (553.9 μs .. 566.1 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 562.2 μs   (556.1 μs .. 568.5 μs)
std dev              20.69 μs   (16.75 μs .. 26.22 μs)
variance introduced by outliers: 29% (moderately inflated)

benchmarking binary-parser/lazy-bytestring/numbers
time                 385.9 μs   (378.3 μs .. 399.4 μs)
                     0.941 R²   (0.847 R² .. 0.999 R²)
mean                 405.0 μs   (383.5 μs .. 480.0 μs)
std dev              116.0 μs   (14.13 μs .. 230.5 μs)
variance introduced by outliers: 98% (severely inflated)

benchmarking attoparsec/twitter1
time                 24.99 μs   (24.71 μs .. 25.24 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 25.12 μs   (24.87 μs .. 25.45 μs)
std dev              973.2 ns   (772.2 ns .. 1.293 μs)
variance introduced by outliers: 45% (moderately inflated)

benchmarking binary-parser/twitter1
time                 18.89 μs   (18.63 μs .. 19.17 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 18.84 μs   (18.67 μs .. 19.07 μs)
std dev              665.9 ns   (527.6 ns .. 870.2 ns)
variance introduced by outliers: 41% (moderately inflated)

benchmarking attoparsec/lazy-bytestring/twitter1
time                 24.80 μs   (24.40 μs .. 25.23 μs)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 24.98 μs   (24.67 μs .. 25.40 μs)
std dev              1.165 μs   (879.9 ns .. 1.845 μs)
variance introduced by outliers: 54% (severely inflated)

benchmarking binary-parser/lazy-bytestring/twitter1
time                 19.14 μs   (18.95 μs .. 19.33 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 19.13 μs   (18.95 μs .. 19.34 μs)
std dev              654.4 ns   (531.6 ns .. 864.0 ns)
variance introduced by outliers: 40% (moderately inflated)

benchmarking attoparsec/twitter10
time                 177.8 μs   (176.8 μs .. 179.1 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 179.5 μs   (178.1 μs .. 181.6 μs)
std dev              5.936 μs   (4.418 μs .. 8.262 μs)
variance introduced by outliers: 30% (moderately inflated)

benchmarking binary-parser/twitter10
time                 141.9 μs   (138.5 μs .. 144.4 μs)
                     0.923 R²   (0.753 R² .. 0.999 R²)
mean                 168.6 μs   (144.3 μs .. 252.2 μs)
std dev              133.9 μs   (7.126 μs .. 276.1 μs)
variance introduced by outliers: 99% (severely inflated)

benchmarking attoparsec/lazy-bytestring/twitter10
time                 174.4 μs   (172.2 μs .. 176.9 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 176.3 μs   (174.6 μs .. 178.7 μs)
std dev              6.816 μs   (5.259 μs .. 9.747 μs)
variance introduced by outliers: 37% (moderately inflated)

benchmarking binary-parser/lazy-bytestring/twitter10
time                 140.9 μs   (139.7 μs .. 142.1 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 141.9 μs   (140.4 μs .. 143.3 μs)
std dev              4.869 μs   (3.996 μs .. 6.329 μs)
variance introduced by outliers: 32% (moderately inflated)

benchmarking attoparsec/twitter100
time                 2.316 ms   (2.278 ms .. 2.348 ms)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 2.311 ms   (2.288 ms .. 2.340 ms)
std dev              86.80 μs   (68.60 μs .. 125.5 μs)
variance introduced by outliers: 23% (moderately inflated)

benchmarking binary-parser/twitter100
time                 2.066 ms   (2.037 ms .. 2.097 ms)
                     0.998 R²   (0.996 R² .. 0.999 R²)
mean                 2.117 ms   (2.089 ms .. 2.149 ms)
std dev              101.3 μs   (82.59 μs .. 122.3 μs)
variance introduced by outliers: 33% (moderately inflated)

benchmarking attoparsec/lazy-bytestring/twitter100
time                 2.596 ms   (2.301 ms .. 3.072 ms)
                     0.921 R²   (0.882 R² .. 0.998 R²)
mean                 2.415 ms   (2.370 ms .. 2.581 ms)
std dev              255.4 μs   (74.33 μs .. 511.0 μs)
variance introduced by outliers: 69% (severely inflated)

benchmarking binary-parser/lazy-bytestring/twitter100
time                 2.070 ms   (2.037 ms .. 2.103 ms)
                     0.997 R²   (0.995 R² .. 0.998 R²)
mean                 2.084 ms   (2.060 ms .. 2.111 ms)
std dev              85.16 μs   (68.84 μs .. 109.8 μs)
variance introduced by outliers: 27% (moderately inflated)

benchmarking attoparsec/twitter20
time                 377.1 μs   (372.9 μs .. 382.8 μs)
                     0.998 R²   (0.998 R² .. 0.999 R²)
mean                 379.3 μs   (375.7 μs .. 383.0 μs)
std dev              12.74 μs   (10.89 μs .. 16.09 μs)
variance introduced by outliers: 28% (moderately inflated)

benchmarking binary-parser/twitter20
time                 336.0 μs   (332.3 μs .. 339.5 μs)
                     0.998 R²   (0.998 R² .. 0.999 R²)
mean                 329.5 μs   (325.6 μs .. 333.5 μs)
std dev              13.34 μs   (11.36 μs .. 15.62 μs)
variance introduced by outliers: 36% (moderately inflated)

benchmarking attoparsec/lazy-bytestring/twitter20
time                 378.6 μs   (375.4 μs .. 382.0 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 379.0 μs   (375.5 μs .. 383.1 μs)
std dev              11.67 μs   (9.355 μs .. 15.51 μs)
variance introduced by outliers: 24% (moderately inflated)

benchmarking binary-parser/lazy-bytestring/twitter20
time                 320.5 μs   (315.1 μs .. 327.5 μs)
                     0.998 R²   (0.995 R² .. 0.999 R²)
mean                 320.7 μs   (317.6 μs .. 324.9 μs)
std dev              11.79 μs   (8.713 μs .. 16.30 μs)
variance introduced by outliers: 32% (moderately inflated)

benchmarking attoparsec/twitter50
time                 1.251 ms   (1.118 ms .. 1.536 ms)
                     0.850 R²   (0.749 R² .. 0.999 R²)
mean                 1.187 ms   (1.146 ms .. 1.299 ms)
std dev              233.9 μs   (41.99 μs .. 447.0 μs)
variance introduced by outliers: 92% (severely inflated)

benchmarking binary-parser/twitter50
time                 1.025 ms   (1.012 ms .. 1.039 ms)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 1.028 ms   (1.019 ms .. 1.039 ms)
std dev              32.69 μs   (24.30 μs .. 43.61 μs)
variance introduced by outliers: 20% (moderately inflated)

benchmarking attoparsec/lazy-bytestring/twitter50
time                 1.113 ms   (1.100 ms .. 1.127 ms)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 1.127 ms   (1.117 ms .. 1.139 ms)
std dev              35.76 μs   (28.73 μs .. 47.10 μs)
variance introduced by outliers: 21% (moderately inflated)

benchmarking binary-parser/lazy-bytestring/twitter50
time                 1.055 ms   (1.039 ms .. 1.071 ms)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 1.042 ms   (1.032 ms .. 1.055 ms)
std dev              36.88 μs   (28.99 μs .. 54.81 μs)
variance introduced by outliers: 25% (moderately inflated)
```
