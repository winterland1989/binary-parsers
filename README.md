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
time                 2.234 μs   (2.216 μs .. 2.252 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 2.243 μs   (2.227 μs .. 2.261 μs)
std dev              58.33 ns   (45.77 ns .. 81.74 ns)
variance introduced by outliers: 32% (moderately inflated)

benchmarking http-req/binary-parsers
time                 1.355 μs   (1.344 μs .. 1.368 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 1.367 μs   (1.354 μs .. 1.379 μs)
std dev              42.42 ns   (34.69 ns .. 54.51 ns)
variance introduced by outliers: 42% (moderately inflated)

benchmarking http-req/scanner
time                 1.440 μs   (1.346 μs .. 1.652 μs)
                     0.928 R²   (0.848 R² .. 0.999 R²)
mean                 1.392 μs   (1.345 μs .. 1.528 μs)
std dev              268.6 ns   (40.18 ns .. 516.8 ns)
variance introduced by outliers: 97% (severely inflated)

benchmarking http-req/warp
time                 921.0 ns   (913.2 ns .. 928.5 ns)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 923.5 ns   (916.3 ns .. 931.3 ns)
std dev              25.51 ns   (21.12 ns .. 31.75 ns)
variance introduced by outliers: 38% (moderately inflated)

start benchmark JSON parser
benchmarking attoparsec/buffer-builder
time                 4.152 ms   (4.088 ms .. 4.201 ms)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 4.256 ms   (4.221 ms .. 4.302 ms)
std dev              128.6 μs   (97.90 μs .. 190.0 μs)
variance introduced by outliers: 13% (moderately inflated)

benchmarking binary-parser/buffer-builder
time                 3.768 ms   (3.663 ms .. 3.910 ms)
                     0.993 R²   (0.986 R² .. 0.998 R²)
mean                 3.815 ms   (3.765 ms .. 3.877 ms)
std dev              175.5 μs   (140.6 μs .. 253.8 μs)
variance introduced by outliers: 27% (moderately inflated)

benchmarking attoparsec/lazy-bytestring/buffer-builder
time                 4.117 ms   (4.037 ms .. 4.186 ms)
                     0.997 R²   (0.994 R² .. 0.999 R²)
mean                 4.328 ms   (4.151 ms .. 5.153 ms)
std dev              1.013 ms   (102.4 μs .. 2.296 ms)
variance introduced by outliers: 90% (severely inflated)

benchmarking binary-parser/lazy-bytestring/buffer-builder
time                 4.146 ms   (3.765 ms .. 4.911 ms)
                     0.763 R²   (0.649 R² .. 0.975 R²)
mean                 4.158 ms   (3.859 ms .. 4.783 ms)
std dev              1.300 ms   (791.0 μs .. 1.930 ms)
variance introduced by outliers: 95% (severely inflated)

benchmarking attoparsec/dates-fract
time                 3.990 μs   (3.934 μs .. 4.064 μs)
                     0.958 R²   (0.869 R² .. 0.998 R²)
mean                 4.678 μs   (4.105 μs .. 6.598 μs)
std dev              3.240 μs   (890.6 ns .. 6.569 μs)
variance introduced by outliers: 99% (severely inflated)

benchmarking binary-parser/dates-fract
time                 3.073 μs   (2.993 μs .. 3.168 μs)
                     0.993 R²   (0.987 R² .. 0.999 R²)
mean                 3.130 μs   (3.030 μs .. 3.534 μs)
std dev              582.4 ns   (196.7 ns .. 1.256 μs)
variance introduced by outliers: 97% (severely inflated)

benchmarking attoparsec/lazy-bytestring/dates-fract
time                 4.162 μs   (4.079 μs .. 4.301 μs)
                     0.936 R²   (0.818 R² .. 0.998 R²)
mean                 4.485 μs   (4.118 μs .. 5.618 μs)
std dev              2.185 μs   (241.3 ns .. 4.168 μs)
variance introduced by outliers: 99% (severely inflated)

benchmarking binary-parser/lazy-bytestring/dates-fract
time                 3.038 μs   (3.017 μs .. 3.061 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 3.042 μs   (3.017 μs .. 3.067 μs)
std dev              86.40 ns   (72.15 ns .. 110.5 ns)
variance introduced by outliers: 36% (moderately inflated)

benchmarking attoparsec/dates
time                 3.890 μs   (3.852 μs .. 3.927 μs)
                     0.999 R²   (0.999 R² .. 0.999 R²)
mean                 3.911 μs   (3.869 μs .. 3.955 μs)
std dev              147.6 ns   (125.4 ns .. 185.8 ns)
variance introduced by outliers: 49% (moderately inflated)

benchmarking binary-parser/dates
time                 2.962 μs   (2.930 μs .. 2.993 μs)
                     0.999 R²   (0.999 R² .. 0.999 R²)
mean                 2.971 μs   (2.940 μs .. 3.007 μs)
std dev              115.6 ns   (92.31 ns .. 143.5 ns)
variance introduced by outliers: 51% (severely inflated)

benchmarking attoparsec/lazy-bytestring/dates
time                 3.950 μs   (3.891 μs .. 4.010 μs)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 3.954 μs   (3.905 μs .. 4.009 μs)
std dev              171.8 ns   (145.4 ns .. 203.0 ns)
variance introduced by outliers: 56% (severely inflated)

benchmarking binary-parser/lazy-bytestring/dates
time                 2.961 μs   (2.935 μs .. 2.987 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 2.960 μs   (2.936 μs .. 2.993 μs)
std dev              93.80 ns   (72.84 ns .. 145.9 ns)
variance introduced by outliers: 41% (moderately inflated)

benchmarking attoparsec/example
time                 74.67 μs   (73.57 μs .. 75.75 μs)
                     0.998 R²   (0.998 R² .. 0.999 R²)
mean                 74.86 μs   (74.11 μs .. 75.73 μs)
std dev              2.666 μs   (2.239 μs .. 3.379 μs)
variance introduced by outliers: 37% (moderately inflated)

benchmarking binary-parser/example
time                 57.40 μs   (56.92 μs .. 57.90 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 57.63 μs   (57.19 μs .. 58.12 μs)
std dev              1.585 μs   (1.240 μs .. 2.004 μs)
variance introduced by outliers: 26% (moderately inflated)

benchmarking attoparsec/lazy-bytestring/example
time                 78.02 μs   (77.31 μs .. 78.81 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 78.12 μs   (77.35 μs .. 78.97 μs)
std dev              2.733 μs   (2.141 μs .. 3.506 μs)
variance introduced by outliers: 35% (moderately inflated)

benchmarking binary-parser/lazy-bytestring/example
time                 62.15 μs   (57.00 μs .. 72.25 μs)
                     0.931 R²   (0.874 R² .. 0.999 R²)
mean                 58.85 μs   (57.24 μs .. 63.61 μs)
std dev              8.903 μs   (2.232 μs .. 18.31 μs)
variance introduced by outliers: 92% (severely inflated)

benchmarking attoparsec/geometry
time                 3.216 ms   (3.166 ms .. 3.266 ms)
                     0.995 R²   (0.986 R² .. 0.999 R²)
mean                 3.293 ms   (3.235 ms .. 3.410 ms)
std dev              258.3 μs   (113.4 μs .. 481.1 μs)
variance introduced by outliers: 53% (severely inflated)

benchmarking binary-parser/geometry
time                 2.466 ms   (2.418 ms .. 2.523 ms)
                     0.996 R²   (0.994 R² .. 0.998 R²)
mean                 2.527 ms   (2.498 ms .. 2.567 ms)
std dev              114.9 μs   (92.96 μs .. 146.6 μs)
variance introduced by outliers: 29% (moderately inflated)

benchmarking attoparsec/lazy-bytestring/geometry
time                 3.296 ms   (3.240 ms .. 3.357 ms)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 3.282 ms   (3.252 ms .. 3.310 ms)
std dev              94.60 μs   (78.03 μs .. 117.8 μs)
variance introduced by outliers: 14% (moderately inflated)

benchmarking binary-parser/lazy-bytestring/geometry
time                 2.462 ms   (2.424 ms .. 2.500 ms)
                     0.998 R²   (0.996 R² .. 0.999 R²)
mean                 2.454 ms   (2.431 ms .. 2.483 ms)
std dev              90.13 μs   (74.45 μs .. 119.6 μs)
variance introduced by outliers: 22% (moderately inflated)

benchmarking attoparsec/integers
time                 303.8 μs   (301.0 μs .. 306.8 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 304.3 μs   (301.6 μs .. 307.0 μs)
std dev              8.985 μs   (7.176 μs .. 11.49 μs)
variance introduced by outliers: 23% (moderately inflated)

benchmarking binary-parser/integers
time                 222.4 μs   (206.7 μs .. 245.6 μs)
                     0.956 R²   (0.907 R² .. 0.998 R²)
mean                 210.2 μs   (205.4 μs .. 226.7 μs)
std dev              26.99 μs   (8.189 μs .. 54.58 μs)
variance introduced by outliers: 86% (severely inflated)

benchmarking attoparsec/lazy-bytestring/integers
time                 302.1 μs   (298.8 μs .. 305.5 μs)
                     0.999 R²   (0.999 R² .. 0.999 R²)
mean                 304.8 μs   (302.9 μs .. 307.2 μs)
std dev              7.239 μs   (5.903 μs .. 8.904 μs)
variance introduced by outliers: 16% (moderately inflated)

benchmarking binary-parser/lazy-bytestring/integers
time                 206.4 μs   (203.0 μs .. 210.2 μs)
                     0.998 R²   (0.998 R² .. 0.999 R²)
mean                 204.7 μs   (203.1 μs .. 206.3 μs)
std dev              5.477 μs   (4.497 μs .. 6.974 μs)
variance introduced by outliers: 21% (moderately inflated)

benchmarking attoparsec/jp10
time                 489.4 μs   (484.9 μs .. 495.0 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 490.0 μs   (486.2 μs .. 494.0 μs)
std dev              13.70 μs   (10.93 μs .. 18.78 μs)
variance introduced by outliers: 20% (moderately inflated)

benchmarking binary-parser/jp10
time                 458.2 μs   (455.1 μs .. 461.6 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 463.4 μs   (459.1 μs .. 468.5 μs)
std dev              15.32 μs   (12.59 μs .. 19.01 μs)
variance introduced by outliers: 26% (moderately inflated)

benchmarking attoparsec/lazy-bytestring/jp10
time                 488.0 μs   (480.6 μs .. 493.8 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 485.8 μs   (481.6 μs .. 490.0 μs)
std dev              14.50 μs   (11.70 μs .. 19.83 μs)
variance introduced by outliers: 22% (moderately inflated)

benchmarking binary-parser/lazy-bytestring/jp10
time                 478.6 μs   (453.6 μs .. 535.7 μs)
                     0.932 R²   (0.837 R² .. 0.999 R²)
mean                 473.8 μs   (458.7 μs .. 527.8 μs)
std dev              84.24 μs   (12.21 μs .. 172.8 μs)
variance introduced by outliers: 91% (severely inflated)

benchmarking attoparsec/jp100
time                 2.961 ms   (2.913 ms .. 3.010 ms)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 3.009 ms   (2.985 ms .. 3.035 ms)
std dev              78.61 μs   (67.01 μs .. 95.10 μs)
variance introduced by outliers: 11% (moderately inflated)

benchmarking binary-parser/jp100
time                 2.735 ms   (2.703 ms .. 2.771 ms)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 2.731 ms   (2.707 ms .. 2.762 ms)
std dev              91.60 μs   (70.79 μs .. 137.6 μs)
variance introduced by outliers: 18% (moderately inflated)

benchmarking attoparsec/lazy-bytestring/jp100
time                 3.005 ms   (2.964 ms .. 3.051 ms)
                     0.998 R²   (0.996 R² .. 0.999 R²)
mean                 3.095 ms   (3.065 ms .. 3.128 ms)
std dev              108.5 μs   (88.97 μs .. 134.4 μs)
variance introduced by outliers: 19% (moderately inflated)

benchmarking binary-parser/lazy-bytestring/jp100
time                 2.760 ms   (2.724 ms .. 2.797 ms)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 2.747 ms   (2.718 ms .. 2.776 ms)
std dev              93.90 μs   (71.25 μs .. 122.5 μs)
variance introduced by outliers: 19% (moderately inflated)

benchmarking attoparsec/jp50
time                 1.618 ms   (1.591 ms .. 1.648 ms)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 1.616 ms   (1.602 ms .. 1.633 ms)
std dev              50.98 μs   (40.28 μs .. 69.45 μs)
variance introduced by outliers: 19% (moderately inflated)

benchmarking binary-parser/jp50
time                 1.518 ms   (1.459 ms .. 1.674 ms)
                     0.863 R²   (0.672 R² .. 0.999 R²)
mean                 1.533 ms   (1.466 ms .. 1.801 ms)
std dev              421.6 μs   (44.69 μs .. 893.1 μs)
variance introduced by outliers: 95% (severely inflated)

benchmarking attoparsec/lazy-bytestring/jp50
time                 1.668 ms   (1.655 ms .. 1.684 ms)
                     0.999 R²   (0.999 R² .. 0.999 R²)
mean                 1.674 ms   (1.664 ms .. 1.690 ms)
std dev              42.55 μs   (34.99 μs .. 54.22 μs)
variance introduced by outliers: 12% (moderately inflated)

benchmarking binary-parser/lazy-bytestring/jp50
time                 1.436 ms   (1.420 ms .. 1.453 ms)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 1.476 ms   (1.462 ms .. 1.490 ms)
std dev              47.49 μs   (39.59 μs .. 57.24 μs)
variance introduced by outliers: 20% (moderately inflated)

benchmarking attoparsec/numbers
time                 549.6 μs   (541.4 μs .. 558.1 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 545.8 μs   (541.5 μs .. 550.4 μs)
std dev              14.23 μs   (11.57 μs .. 18.70 μs)
variance introduced by outliers: 17% (moderately inflated)

benchmarking binary-parser/numbers
time                 365.3 μs   (361.6 μs .. 369.2 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 366.2 μs   (363.1 μs .. 369.7 μs)
std dev              11.05 μs   (9.100 μs .. 13.57 μs)
variance introduced by outliers: 23% (moderately inflated)

benchmarking attoparsec/lazy-bytestring/numbers
time                 550.6 μs   (545.9 μs .. 554.0 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 547.2 μs   (542.7 μs .. 551.8 μs)
std dev              14.99 μs   (12.38 μs .. 19.36 μs)
variance introduced by outliers: 19% (moderately inflated)

benchmarking binary-parser/lazy-bytestring/numbers
time                 370.4 μs   (364.8 μs .. 379.3 μs)
                     0.932 R²   (0.791 R² .. 0.999 R²)
mean                 397.1 μs   (371.9 μs .. 490.7 μs)
std dev              152.8 μs   (12.14 μs .. 323.0 μs)
variance introduced by outliers: 99% (severely inflated)

benchmarking attoparsec/twitter1
time                 24.33 μs   (24.01 μs .. 24.61 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 24.11 μs   (23.86 μs .. 24.38 μs)
std dev              907.2 ns   (757.5 ns .. 1.133 μs)
variance introduced by outliers: 43% (moderately inflated)

benchmarking binary-parser/twitter1
time                 18.40 μs   (18.27 μs .. 18.54 μs)
                     0.999 R²   (0.999 R² .. 0.999 R²)
mean                 18.39 μs   (18.19 μs .. 18.58 μs)
std dev              657.9 ns   (537.6 ns .. 849.5 ns)
variance introduced by outliers: 42% (moderately inflated)

benchmarking attoparsec/lazy-bytestring/twitter1
time                 24.34 μs   (24.10 μs .. 24.62 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 24.30 μs   (24.00 μs .. 24.57 μs)
std dev              959.3 ns   (787.1 ns .. 1.229 μs)
variance introduced by outliers: 46% (moderately inflated)

benchmarking binary-parser/lazy-bytestring/twitter1
time                 18.27 μs   (18.11 μs .. 18.45 μs)
                     0.999 R²   (0.999 R² .. 0.999 R²)
mean                 18.43 μs   (18.25 μs .. 18.65 μs)
std dev              627.3 ns   (509.6 ns .. 763.0 ns)
variance introduced by outliers: 39% (moderately inflated)

benchmarking attoparsec/twitter10
time                 177.9 μs   (175.8 μs .. 179.7 μs)
                     0.999 R²   (0.999 R² .. 0.999 R²)
mean                 176.6 μs   (175.0 μs .. 178.3 μs)
std dev              5.613 μs   (4.525 μs .. 7.147 μs)
variance introduced by outliers: 28% (moderately inflated)

benchmarking binary-parser/twitter10
time                 137.7 μs   (133.9 μs .. 141.0 μs)
                     0.974 R²   (0.940 R² .. 0.996 R²)
mean                 217.6 μs   (158.7 μs .. 343.3 μs)
std dev              289.1 μs   (104.6 μs .. 496.4 μs)
variance introduced by outliers: 99% (severely inflated)

benchmarking attoparsec/lazy-bytestring/twitter10
time                 172.6 μs   (171.0 μs .. 174.1 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 174.4 μs   (172.5 μs .. 176.7 μs)
std dev              7.031 μs   (5.406 μs .. 9.696 μs)
variance introduced by outliers: 39% (moderately inflated)

benchmarking binary-parser/lazy-bytestring/twitter10
time                 138.8 μs   (137.9 μs .. 139.9 μs)
                     0.999 R²   (0.999 R² .. 0.999 R²)
mean                 140.6 μs   (139.2 μs .. 142.2 μs)
std dev              4.915 μs   (3.984 μs .. 6.230 μs)
variance introduced by outliers: 33% (moderately inflated)

benchmarking attoparsec/twitter100
time                 2.273 ms   (2.236 ms .. 2.301 ms)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 2.276 ms   (2.258 ms .. 2.299 ms)
std dev              69.99 μs   (50.91 μs .. 109.2 μs)
variance introduced by outliers: 17% (moderately inflated)

benchmarking binary-parser/twitter100
time                 2.035 ms   (2.007 ms .. 2.059 ms)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 2.056 ms   (2.034 ms .. 2.078 ms)
std dev              74.85 μs   (62.02 μs .. 88.08 μs)
variance introduced by outliers: 22% (moderately inflated)

benchmarking attoparsec/lazy-bytestring/twitter100
time                 2.550 ms   (2.322 ms .. 2.870 ms)
                     0.960 R²   (0.931 R² .. 0.998 R²)
mean                 2.359 ms   (2.322 ms .. 2.459 ms)
std dev              194.3 μs   (75.87 μs .. 386.2 μs)
variance introduced by outliers: 58% (severely inflated)

benchmarking binary-parser/lazy-bytestring/twitter100
time                 2.016 ms   (1.983 ms .. 2.050 ms)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 2.052 ms   (2.031 ms .. 2.078 ms)
std dev              80.58 μs   (63.40 μs .. 112.5 μs)
variance introduced by outliers: 25% (moderately inflated)

benchmarking attoparsec/twitter20
time                 377.3 μs   (373.5 μs .. 380.8 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 372.9 μs   (369.3 μs .. 376.9 μs)
std dev              12.50 μs   (10.42 μs .. 15.59 μs)
variance introduced by outliers: 28% (moderately inflated)

benchmarking binary-parser/twitter20
time                 331.1 μs   (327.1 μs .. 335.0 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 329.2 μs   (325.8 μs .. 332.4 μs)
std dev              11.08 μs   (9.652 μs .. 13.51 μs)
variance introduced by outliers: 28% (moderately inflated)

benchmarking attoparsec/lazy-bytestring/twitter20
time                 375.3 μs   (372.2 μs .. 377.7 μs)
                     0.999 R²   (0.999 R² .. 0.999 R²)
mean                 372.5 μs   (369.2 μs .. 376.2 μs)
std dev              11.27 μs   (9.275 μs .. 14.00 μs)
variance introduced by outliers: 23% (moderately inflated)

benchmarking binary-parser/lazy-bytestring/twitter20
time                 333.5 μs   (330.7 μs .. 336.2 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 334.0 μs   (330.9 μs .. 337.5 μs)
std dev              10.37 μs   (8.658 μs .. 13.04 μs)
variance introduced by outliers: 25% (moderately inflated)

benchmarking attoparsec/twitter50
time                 1.247 ms   (1.125 ms .. 1.524 ms)
                     0.876 R²   (0.782 R² .. 0.999 R²)
mean                 1.169 ms   (1.134 ms .. 1.326 ms)
std dev              208.9 μs   (24.07 μs .. 477.5 μs)
variance introduced by outliers: 89% (severely inflated)

benchmarking binary-parser/twitter50
time                 1.020 ms   (1.008 ms .. 1.031 ms)
                     0.998 R²   (0.998 R² .. 0.999 R²)
mean                 1.032 ms   (1.023 ms .. 1.042 ms)
std dev              32.65 μs   (26.99 μs .. 40.35 μs)
variance introduced by outliers: 20% (moderately inflated)

benchmarking attoparsec/lazy-bytestring/twitter50
time                 1.105 ms   (1.085 ms .. 1.119 ms)
                     0.997 R²   (0.996 R² .. 0.998 R²)
mean                 1.144 ms   (1.122 ms .. 1.176 ms)
std dev              88.80 μs   (68.65 μs .. 110.9 μs)
variance introduced by outliers: 60% (severely inflated)

benchmarking binary-parser/lazy-bytestring/twitter50
time                 1.026 ms   (1.015 ms .. 1.038 ms)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 1.035 ms   (1.027 ms .. 1.043 ms)
std dev              28.09 μs   (22.72 μs .. 36.10 μs)
variance introduced by outliers: 16% (moderately inflated)

Benchmark criterion: FINISH
```
