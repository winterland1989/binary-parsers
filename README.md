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
benchmarking http-req/attoparsec
time                 2.240 μs   (2.216 μs .. 2.264 μs)
                     0.998 R²   (0.996 R² .. 0.999 R²)
mean                 2.320 μs   (2.251 μs .. 2.702 μs)
std dev              417.9 ns   (60.72 ns .. 1.012 μs)
variance introduced by outliers: 96% (severely inflated)

benchmarking http-req/binary-parsers
time                 1.552 μs   (1.533 μs .. 1.577 μs)
                     0.980 R²   (0.942 R² .. 0.999 R²)
mean                 1.673 μs   (1.549 μs .. 1.984 μs)
std dev              583.2 ns   (53.70 ns .. 1.115 μs)
variance introduced by outliers: 99% (severely inflated)

benchmarking http-req/warp
time                 903.0 ns   (894.7 ns .. 911.2 ns)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 906.6 ns   (896.2 ns .. 918.4 ns)
std dev              36.38 ns   (28.49 ns .. 51.58 ns)
variance introduced by outliers: 56% (severely inflated)

Benchmark criterion: RUNNING...
benchmarking attoparsec/buffer-builder
time                 4.025 ms   (3.965 ms .. 4.097 ms)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 4.180 ms   (4.133 ms .. 4.242 ms)
std dev              164.2 μs   (126.7 μs .. 204.4 μs)
variance introduced by outliers: 22% (moderately inflated)

benchmarking binary-parser/buffer-builder
time                 3.509 ms   (3.462 ms .. 3.552 ms)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 3.585 ms   (3.550 ms .. 3.646 ms)
std dev              151.8 μs   (85.69 μs .. 273.2 μs)
variance introduced by outliers: 24% (moderately inflated)

benchmarking attoparsec/lazy-bytestring/buffer-builder
time                 4.360 ms   (4.192 ms .. 4.641 ms)
                     0.970 R²   (0.936 R² .. 0.996 R²)
mean                 4.355 ms   (4.254 ms .. 4.546 ms)
std dev              408.3 μs   (258.9 μs .. 687.3 μs)
variance introduced by outliers: 59% (severely inflated)

benchmarking binary-parser/lazy-bytestring/buffer-builder
time                 3.541 ms   (3.476 ms .. 3.602 ms)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 3.560 ms   (3.525 ms .. 3.607 ms)
std dev              127.2 μs   (98.15 μs .. 170.4 μs)
variance introduced by outliers: 18% (moderately inflated)

benchmarking attoparsec/dates-fract
time                 4.266 μs   (4.112 μs .. 4.506 μs)
                     0.979 R²   (0.945 R² .. 0.999 R²)
mean                 4.184 μs   (4.099 μs .. 4.485 μs)
std dev              495.3 ns   (141.5 ns .. 1.023 μs)
variance introduced by outliers: 91% (severely inflated)

benchmarking binary-parser/dates-fract
time                 3.179 μs   (3.151 μs .. 3.206 μs)
                     0.999 R²   (0.999 R² .. 0.999 R²)
mean                 3.192 μs   (3.161 μs .. 3.224 μs)
std dev              106.0 ns   (88.88 ns .. 130.2 ns)
variance introduced by outliers: 43% (moderately inflated)

benchmarking attoparsec/lazy-bytestring/dates-fract
time                 4.176 μs   (4.140 μs .. 4.213 μs)
                     0.999 R²   (0.999 R² .. 0.999 R²)
mean                 4.158 μs   (4.115 μs .. 4.194 μs)
std dev              126.3 ns   (102.9 ns .. 156.6 ns)
variance introduced by outliers: 38% (moderately inflated)

benchmarking binary-parser/lazy-bytestring/dates-fract
time                 3.243 μs   (3.199 μs .. 3.290 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 3.225 μs   (3.193 μs .. 3.259 μs)
std dev              108.6 ns   (90.97 ns .. 130.0 ns)
variance introduced by outliers: 44% (moderately inflated)

benchmarking attoparsec/dates
time                 3.912 μs   (3.866 μs .. 3.962 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 3.897 μs   (3.858 μs .. 3.940 μs)
std dev              134.6 ns   (115.1 ns .. 161.1 ns)
variance introduced by outliers: 44% (moderately inflated)

benchmarking binary-parser/dates
time                 3.049 μs   (3.030 μs .. 3.068 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 3.044 μs   (3.017 μs .. 3.083 μs)
std dev              100.4 ns   (78.69 ns .. 158.1 ns)
variance introduced by outliers: 43% (moderately inflated)

benchmarking attoparsec/lazy-bytestring/dates
time                 4.063 μs   (3.949 μs .. 4.309 μs)
                     0.971 R²   (0.914 R² .. 0.999 R²)
mean                 4.086 μs   (3.967 μs .. 4.648 μs)
std dev              698.0 ns   (100.7 ns .. 1.573 μs)
variance introduced by outliers: 95% (severely inflated)

benchmarking binary-parser/lazy-bytestring/dates
time                 3.119 μs   (3.090 μs .. 3.144 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 3.136 μs   (3.104 μs .. 3.177 μs)
std dev              117.7 ns   (89.99 ns .. 159.5 ns)
variance introduced by outliers: 50% (moderately inflated)

benchmarking attoparsec/example
time                 72.13 μs   (70.94 μs .. 73.31 μs)
                     0.998 R²   (0.998 R² .. 0.999 R²)
mean                 72.33 μs   (71.62 μs .. 73.14 μs)
std dev              2.436 μs   (2.064 μs .. 2.884 μs)
variance introduced by outliers: 34% (moderately inflated)

benchmarking binary-parser/example
time                 56.33 μs   (55.34 μs .. 57.30 μs)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 56.49 μs   (55.79 μs .. 57.23 μs)
std dev              2.350 μs   (2.001 μs .. 2.837 μs)
variance introduced by outliers: 45% (moderately inflated)

benchmarking attoparsec/lazy-bytestring/example
time                 77.40 μs   (76.73 μs .. 78.14 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 76.89 μs   (76.17 μs .. 77.52 μs)
std dev              2.279 μs   (1.928 μs .. 2.805 μs)
variance introduced by outliers: 28% (moderately inflated)

benchmarking binary-parser/lazy-bytestring/example
time                 55.60 μs   (54.87 μs .. 56.35 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 55.96 μs   (55.38 μs .. 56.61 μs)
std dev              2.079 μs   (1.695 μs .. 2.696 μs)
variance introduced by outliers: 40% (moderately inflated)

benchmarking attoparsec/geometry
time                 3.276 ms   (3.193 ms .. 3.358 ms)
                     0.956 R²   (0.864 R² .. 0.999 R²)
mean                 3.469 ms   (3.348 ms .. 3.945 ms)
std dev              643.7 μs   (126.3 μs .. 1.290 ms)
variance introduced by outliers: 86% (severely inflated)

benchmarking binary-parser/geometry
time                 2.391 ms   (2.365 ms .. 2.414 ms)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 2.400 ms   (2.382 ms .. 2.422 ms)
std dev              67.04 μs   (55.68 μs .. 89.92 μs)
variance introduced by outliers: 14% (moderately inflated)

benchmarking attoparsec/lazy-bytestring/geometry
time                 3.310 ms   (3.268 ms .. 3.358 ms)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 3.331 ms   (3.300 ms .. 3.365 ms)
std dev              98.28 μs   (81.46 μs .. 121.5 μs)
variance introduced by outliers: 14% (moderately inflated)

benchmarking binary-parser/lazy-bytestring/geometry
time                 2.367 ms   (2.340 ms .. 2.403 ms)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 2.381 ms   (2.361 ms .. 2.403 ms)
std dev              74.51 μs   (61.89 μs .. 90.25 μs)
variance introduced by outliers: 18% (moderately inflated)

benchmarking attoparsec/integers
time                 288.9 μs   (286.7 μs .. 290.7 μs)
                     0.999 R²   (0.999 R² .. 0.999 R²)
mean                 290.2 μs   (287.6 μs .. 293.5 μs)
std dev              9.951 μs   (7.963 μs .. 12.97 μs)
variance introduced by outliers: 29% (moderately inflated)

benchmarking binary-parser/integers
time                 160.9 μs   (159.3 μs .. 162.5 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 159.9 μs   (158.6 μs .. 161.4 μs)
std dev              4.600 μs   (3.712 μs .. 5.996 μs)
variance introduced by outliers: 25% (moderately inflated)

benchmarking attoparsec/lazy-bytestring/integers
time                 291.4 μs   (284.9 μs .. 297.8 μs)
                     0.981 R²   (0.947 R² .. 0.998 R²)
mean                 333.5 μs   (298.2 μs .. 439.6 μs)
std dev              168.6 μs   (12.37 μs .. 334.8 μs)
variance introduced by outliers: 99% (severely inflated)

benchmarking binary-parser/lazy-bytestring/integers
time                 162.9 μs   (160.7 μs .. 165.5 μs)
                     0.998 R²   (0.998 R² .. 0.999 R²)
mean                 163.0 μs   (161.3 μs .. 165.4 μs)
std dev              6.593 μs   (5.433 μs .. 8.570 μs)
variance introduced by outliers: 40% (moderately inflated)

benchmarking attoparsec/jp10
time                 482.6 μs   (478.1 μs .. 488.4 μs)
                     0.997 R²   (0.994 R² .. 0.999 R²)
mean                 493.1 μs   (487.6 μs .. 501.3 μs)
std dev              23.78 μs   (16.67 μs .. 38.12 μs)
variance introduced by outliers: 42% (moderately inflated)

benchmarking binary-parser/jp10
time                 469.5 μs   (464.7 μs .. 474.2 μs)
                     0.999 R²   (0.999 R² .. 0.999 R²)
mean                 473.4 μs   (469.2 μs .. 477.6 μs)
std dev              13.50 μs   (10.90 μs .. 17.13 μs)
variance introduced by outliers: 20% (moderately inflated)

benchmarking attoparsec/lazy-bytestring/jp10
time                 484.6 μs   (478.4 μs .. 489.7 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 486.6 μs   (482.1 μs .. 493.4 μs)
std dev              18.89 μs   (13.73 μs .. 27.58 μs)
variance introduced by outliers: 33% (moderately inflated)

benchmarking binary-parser/lazy-bytestring/jp10
time                 467.7 μs   (461.8 μs .. 472.8 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 467.9 μs   (463.6 μs .. 472.4 μs)
std dev              15.49 μs   (13.18 μs .. 18.54 μs)
variance introduced by outliers: 26% (moderately inflated)

benchmarking attoparsec/jp100
time                 2.971 ms   (2.942 ms .. 3.005 ms)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 3.022 ms   (2.997 ms .. 3.051 ms)
std dev              89.08 μs   (74.72 μs .. 111.6 μs)
variance introduced by outliers: 15% (moderately inflated)

benchmarking binary-parser/jp100
time                 2.607 ms   (2.564 ms .. 2.652 ms)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 2.606 ms   (2.579 ms .. 2.630 ms)
std dev              79.86 μs   (67.90 μs .. 97.65 μs)
variance introduced by outliers: 16% (moderately inflated)

benchmarking attoparsec/lazy-bytestring/jp100
time                 3.028 ms   (2.986 ms .. 3.065 ms)
                     0.998 R²   (0.996 R² .. 0.999 R²)
mean                 3.073 ms   (3.046 ms .. 3.109 ms)
std dev              102.4 μs   (80.67 μs .. 139.1 μs)
variance introduced by outliers: 17% (moderately inflated)

benchmarking binary-parser/lazy-bytestring/jp100
time                 2.617 ms   (2.568 ms .. 2.658 ms)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 2.588 ms   (2.560 ms .. 2.619 ms)
std dev              94.41 μs   (78.93 μs .. 115.4 μs)
variance introduced by outliers: 20% (moderately inflated)

benchmarking attoparsec/jp50
time                 1.575 ms   (1.558 ms .. 1.591 ms)
                     0.998 R²   (0.998 R² .. 0.999 R²)
mean                 1.578 ms   (1.564 ms .. 1.594 ms)
std dev              52.98 μs   (44.55 μs .. 63.68 μs)
variance introduced by outliers: 21% (moderately inflated)

benchmarking binary-parser/jp50
time                 1.541 ms   (1.434 ms .. 1.744 ms)
                     0.957 R²   (0.923 R² .. 0.999 R²)
mean                 1.457 ms   (1.433 ms .. 1.555 ms)
std dev              130.2 μs   (39.54 μs .. 284.7 μs)
variance introduced by outliers: 65% (severely inflated)

benchmarking attoparsec/lazy-bytestring/jp50
time                 1.597 ms   (1.574 ms .. 1.625 ms)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 1.617 ms   (1.603 ms .. 1.632 ms)
std dev              51.29 μs   (42.45 μs .. 62.77 μs)
variance introduced by outliers: 19% (moderately inflated)

benchmarking binary-parser/lazy-bytestring/jp50
time                 1.426 ms   (1.399 ms .. 1.458 ms)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 1.431 ms   (1.419 ms .. 1.446 ms)
std dev              47.52 μs   (39.10 μs .. 56.91 μs)
variance introduced by outliers: 21% (moderately inflated)

benchmarking attoparsec/numbers
time                 550.2 μs   (542.0 μs .. 558.2 μs)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 555.0 μs   (548.9 μs .. 566.3 μs)
std dev              26.67 μs   (15.70 μs .. 47.26 μs)
variance introduced by outliers: 41% (moderately inflated)

benchmarking binary-parser/numbers
time                 353.7 μs   (349.1 μs .. 358.3 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 355.1 μs   (352.0 μs .. 358.6 μs)
std dev              11.49 μs   (9.664 μs .. 14.16 μs)
variance introduced by outliers: 26% (moderately inflated)

benchmarking attoparsec/lazy-bytestring/numbers
time                 555.6 μs   (547.5 μs .. 563.2 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 557.2 μs   (551.7 μs .. 562.6 μs)
std dev              17.77 μs   (15.35 μs .. 20.77 μs)
variance introduced by outliers: 23% (moderately inflated)

benchmarking binary-parser/lazy-bytestring/numbers
time                 374.7 μs   (358.4 μs .. 405.1 μs)
                     0.973 R²   (0.938 R² .. 0.999 R²)
mean                 362.9 μs   (357.1 μs .. 385.7 μs)
std dev              33.84 μs   (9.270 μs .. 69.65 μs)
variance introduced by outliers: 75% (severely inflated)

benchmarking attoparsec/twitter1
time                 23.99 μs   (23.79 μs .. 24.22 μs)
                     0.999 R²   (0.999 R² .. 0.999 R²)
mean                 23.85 μs   (23.61 μs .. 24.08 μs)
std dev              785.0 ns   (666.1 ns .. 941.3 ns)
variance introduced by outliers: 37% (moderately inflated)

benchmarking binary-parser/twitter1
time                 17.50 μs   (17.24 μs .. 17.74 μs)
                     0.998 R²   (0.998 R² .. 0.999 R²)
mean                 17.63 μs   (17.38 μs .. 17.87 μs)
std dev              794.7 ns   (649.8 ns .. 962.0 ns)
variance introduced by outliers: 53% (severely inflated)

benchmarking attoparsec/lazy-bytestring/twitter1
time                 23.77 μs   (23.55 μs .. 23.96 μs)
                     0.999 R²   (0.999 R² .. 0.999 R²)
mean                 23.91 μs   (23.67 μs .. 24.18 μs)
std dev              838.7 ns   (646.4 ns .. 1.126 μs)
variance introduced by outliers: 40% (moderately inflated)

benchmarking binary-parser/lazy-bytestring/twitter1
time                 17.30 μs   (17.17 μs .. 17.42 μs)
                     0.999 R²   (0.999 R² .. 0.999 R²)
mean                 17.31 μs   (17.15 μs .. 17.49 μs)
std dev              557.8 ns   (451.0 ns .. 701.1 ns)
variance introduced by outliers: 37% (moderately inflated)

benchmarking attoparsec/twitter10
time                 166.0 μs   (164.0 μs .. 168.0 μs)
                     0.998 R²   (0.998 R² .. 0.999 R²)
mean                 166.2 μs   (164.3 μs .. 168.2 μs)
std dev              6.722 μs   (5.565 μs .. 8.233 μs)
variance introduced by outliers: 40% (moderately inflated)

benchmarking binary-parser/twitter10
time                 129.0 μs   (125.0 μs .. 136.1 μs)
                     0.968 R²   (0.914 R² .. 0.999 R²)
mean                 130.8 μs   (127.4 μs .. 143.7 μs)
std dev              19.73 μs   (4.362 μs .. 41.31 μs)
variance introduced by outliers: 91% (severely inflated)

benchmarking attoparsec/lazy-bytestring/twitter10
time                 165.5 μs   (163.8 μs .. 167.2 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 167.4 μs   (165.4 μs .. 169.5 μs)
std dev              6.295 μs   (5.242 μs .. 7.620 μs)
variance introduced by outliers: 36% (moderately inflated)

benchmarking binary-parser/lazy-bytestring/twitter10
time                 131.0 μs   (128.5 μs .. 134.4 μs)
                     0.997 R²   (0.995 R² .. 0.999 R²)
mean                 129.8 μs   (128.5 μs .. 131.8 μs)
std dev              5.415 μs   (3.935 μs .. 7.494 μs)
variance introduced by outliers: 42% (moderately inflated)

benchmarking attoparsec/twitter100
time                 2.183 ms   (2.148 ms .. 2.220 ms)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 2.197 ms   (2.175 ms .. 2.224 ms)
std dev              88.02 μs   (64.96 μs .. 133.4 μs)
variance introduced by outliers: 26% (moderately inflated)

benchmarking binary-parser/twitter100
time                 1.865 ms   (1.839 ms .. 1.890 ms)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 1.873 ms   (1.852 ms .. 1.897 ms)
std dev              74.75 μs   (60.54 μs .. 95.21 μs)
variance introduced by outliers: 26% (moderately inflated)

benchmarking attoparsec/lazy-bytestring/twitter100
time                 2.251 ms   (2.221 ms .. 2.284 ms)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 2.287 ms   (2.267 ms .. 2.310 ms)
std dev              73.91 μs   (55.21 μs .. 97.13 μs)
variance introduced by outliers: 17% (moderately inflated)

benchmarking binary-parser/lazy-bytestring/twitter100
time                 1.854 ms   (1.802 ms .. 1.886 ms)
                     0.963 R²   (0.870 R² .. 0.999 R²)
mean                 1.992 ms   (1.905 ms .. 2.391 ms)
std dev              509.8 μs   (62.89 μs .. 1.153 ms)
variance introduced by outliers: 94% (severely inflated)

benchmarking attoparsec/twitter20
time                 359.4 μs   (354.8 μs .. 364.8 μs)
                     0.998 R²   (0.998 R² .. 0.999 R²)
mean                 361.2 μs   (357.4 μs .. 364.7 μs)
std dev              12.05 μs   (10.01 μs .. 14.71 μs)
variance introduced by outliers: 27% (moderately inflated)

benchmarking binary-parser/twitter20
time                 311.8 μs   (307.3 μs .. 315.7 μs)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 310.6 μs   (307.1 μs .. 314.6 μs)
std dev              12.47 μs   (9.920 μs .. 16.24 μs)
variance introduced by outliers: 36% (moderately inflated)

benchmarking attoparsec/lazy-bytestring/twitter20
time                 361.3 μs   (357.9 μs .. 364.9 μs)
                     0.999 R²   (0.999 R² .. 0.999 R²)
mean                 362.2 μs   (359.4 μs .. 365.5 μs)
std dev              9.791 μs   (7.821 μs .. 13.13 μs)
variance introduced by outliers: 20% (moderately inflated)

benchmarking binary-parser/lazy-bytestring/twitter20
time                 305.9 μs   (303.5 μs .. 308.5 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 309.5 μs   (306.4 μs .. 313.3 μs)
std dev              11.72 μs   (9.567 μs .. 15.23 μs)
variance introduced by outliers: 34% (moderately inflated)

benchmarking attoparsec/twitter50
time                 1.074 ms   (1.065 ms .. 1.084 ms)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 1.079 ms   (1.069 ms .. 1.092 ms)
std dev              40.33 μs   (30.96 μs .. 54.69 μs)
variance introduced by outliers: 26% (moderately inflated)

benchmarking binary-parser/twitter50
time                 943.5 μs   (930.2 μs .. 956.8 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 956.1 μs   (948.7 μs .. 964.3 μs)
std dev              25.24 μs   (20.12 μs .. 31.51 μs)
variance introduced by outliers: 16% (moderately inflated)

benchmarking attoparsec/lazy-bytestring/twitter50
time                 1.051 ms   (1.041 ms .. 1.064 ms)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 1.061 ms   (1.051 ms .. 1.070 ms)
std dev              33.20 μs   (27.71 μs .. 41.49 μs)
variance introduced by outliers: 21% (moderately inflated)

benchmarking binary-parser/lazy-bytestring/twitter50
time                 948.1 μs   (925.7 μs .. 975.2 μs)
                     0.996 R²   (0.994 R² .. 0.998 R²)
mean                 947.5 μs   (937.1 μs .. 959.5 μs)
std dev              38.20 μs   (31.36 μs .. 47.85 μs)
variance introduced by outliers: 31% (moderately inflated)

Benchmark criterion: FINISH
```
