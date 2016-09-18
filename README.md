binary-parsers
==============

This package extends [binary](http://hackage.haskell.org/package/binary) with parsec/attoparsec style parsing combinators.

Haskell's binary package is an excellent choice for bytestring parsing, and the central to haskell's eco-system. Contrary to most misbelief, binary's design allows very fast non-backtracking parsing without book-keeping, while still allows backtracking parsing with little extra overhead. By constructing parsing combinators upon binary, i hope to meet following targets:

+ unify haskell's bytestring parsing toolkit.
+ easier to write `Binary` instances with comprehensive combinators.
+ reduce overall maintaining efforts.

Early benchmark showed that it can even exceed attoparsec under some circumstances, please join and help!

Benchmarks
----------

```
Benchmark criterion: RUNNING...
benchmarking aeson/buffer-builder
time                 4.294 ms   (4.238 ms .. 4.343 ms)
                     0.999 R²   (0.997 R² .. 0.999 R²)
mean                 4.307 ms   (4.271 ms .. 4.353 ms)
std dev              129.2 μs   (99.46 μs .. 187.3 μs)
variance introduced by outliers: 13% (moderately inflated)

benchmarking aeson/dates-fract
time                 4.135 μs   (4.096 μs .. 4.171 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 4.120 μs   (4.088 μs .. 4.159 μs)
std dev              118.9 ns   (96.69 ns .. 147.4 ns)
variance introduced by outliers: 36% (moderately inflated)

benchmarking aeson/dates
time                 3.917 μs   (3.879 μs .. 3.959 μs)
                     0.999 R²   (0.999 R² .. 0.999 R²)
mean                 3.921 μs   (3.882 μs .. 3.959 μs)
std dev              128.4 ns   (106.1 ns .. 158.1 ns)
variance introduced by outliers: 42% (moderately inflated)

benchmarking aeson/example
time                 74.65 μs   (73.77 μs .. 75.54 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 74.67 μs   (73.91 μs .. 75.48 μs)
std dev              2.676 μs   (2.139 μs .. 3.817 μs)
variance introduced by outliers: 37% (moderately inflated)

benchmarking aeson/geometry
time                 3.376 ms   (3.299 ms .. 3.511 ms)
                     0.991 R²   (0.979 R² .. 0.999 R²)
mean                 3.358 ms   (3.317 ms .. 3.440 ms)
std dev              173.8 μs   (108.4 μs .. 307.3 μs)
variance introduced by outliers: 32% (moderately inflated)

benchmarking aeson/integers
time                 294.9 μs   (291.9 μs .. 297.6 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 293.2 μs   (290.6 μs .. 295.8 μs)
std dev              8.942 μs   (7.242 μs .. 11.55 μs)
variance introduced by outliers: 24% (moderately inflated)

benchmarking aeson/jp10
time                 478.3 μs   (473.0 μs .. 484.8 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 479.1 μs   (474.8 μs .. 483.9 μs)
std dev              15.15 μs   (12.54 μs .. 20.16 μs)
variance introduced by outliers: 24% (moderately inflated)

benchmarking aeson/jp100
time                 3.008 ms   (2.956 ms .. 3.070 ms)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 3.014 ms   (2.986 ms .. 3.043 ms)
std dev              96.15 μs   (82.52 μs .. 115.9 μs)
variance introduced by outliers: 17% (moderately inflated)

benchmarking aeson/jp50
time                 1.570 ms   (1.551 ms .. 1.585 ms)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 1.565 ms   (1.552 ms .. 1.576 ms)
std dev              41.49 μs   (34.51 μs .. 50.29 μs)
variance introduced by outliers: 14% (moderately inflated)

benchmarking aeson/numbers
time                 554.9 μs   (549.5 μs .. 561.5 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 558.7 μs   (554.1 μs .. 563.1 μs)
std dev              15.41 μs   (12.85 μs .. 19.17 μs)
variance introduced by outliers: 19% (moderately inflated)

benchmarking aeson/twitter1
time                 24.39 μs   (23.91 μs .. 25.21 μs)
                     0.984 R²   (0.953 R² .. 0.999 R²)
mean                 24.86 μs   (24.28 μs .. 26.79 μs)
std dev              3.370 μs   (824.7 ns .. 6.980 μs)
variance introduced by outliers: 91% (severely inflated)

benchmarking aeson/twitter10
time                 177.4 μs   (175.8 μs .. 178.9 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 178.0 μs   (176.1 μs .. 180.3 μs)
std dev              7.169 μs   (5.310 μs .. 9.978 μs)
variance introduced by outliers: 39% (moderately inflated)

benchmarking aeson/twitter100
time                 2.193 ms   (2.163 ms .. 2.222 ms)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 2.191 ms   (2.172 ms .. 2.218 ms)
std dev              73.69 μs   (57.00 μs .. 108.8 μs)
variance introduced by outliers: 19% (moderately inflated)

benchmarking aeson/twitter20
time                 369.7 μs   (365.8 μs .. 374.4 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 367.7 μs   (364.0 μs .. 371.9 μs)
std dev              13.34 μs   (11.10 μs .. 16.24 μs)
variance introduced by outliers: 31% (moderately inflated)

benchmarking aeson/twitter50
time                 1.082 ms   (1.067 ms .. 1.099 ms)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 1.084 ms   (1.075 ms .. 1.096 ms)
std dev              34.53 μs   (28.23 μs .. 49.61 μs)
variance introduced by outliers: 21% (moderately inflated)

benchmarking aeson-binary-parser/buffer-builder
time                 4.088 ms   (4.000 ms .. 4.196 ms)
                     0.997 R²   (0.995 R² .. 0.998 R²)
mean                 4.113 ms   (4.071 ms .. 4.163 ms)
std dev              148.3 μs   (120.1 μs .. 185.5 μs)
variance introduced by outliers: 17% (moderately inflated)

benchmarking aeson-binary-parser/dates-fract
time                 3.479 μs   (3.443 μs .. 3.520 μs)
                     0.991 R²   (0.970 R² .. 1.000 R²)
mean                 3.582 μs   (3.478 μs .. 4.044 μs)
std dev              636.6 ns   (103.7 ns .. 1.452 μs)
variance introduced by outliers: 96% (severely inflated)

benchmarking aeson-binary-parser/dates
time                 3.373 μs   (3.348 μs .. 3.400 μs)
                     0.999 R²   (0.999 R² .. 0.999 R²)
mean                 3.377 μs   (3.340 μs .. 3.414 μs)
std dev              120.1 ns   (101.4 ns .. 144.6 ns)
variance introduced by outliers: 46% (moderately inflated)

benchmarking aeson-binary-parser/example
time                 67.29 μs   (66.45 μs .. 67.95 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 67.79 μs   (67.07 μs .. 68.51 μs)
std dev              2.429 μs   (2.080 μs .. 3.046 μs)
variance introduced by outliers: 37% (moderately inflated)

benchmarking aeson-binary-parser/geometry
time                 3.376 ms   (3.330 ms .. 3.420 ms)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 3.413 ms   (3.387 ms .. 3.436 ms)
std dev              82.15 μs   (64.53 μs .. 109.5 μs)

benchmarking aeson-binary-parser/integers
time                 276.9 μs   (272.0 μs .. 282.8 μs)
                     0.996 R²   (0.991 R² .. 0.999 R²)
mean                 275.4 μs   (272.2 μs .. 280.6 μs)
std dev              13.53 μs   (9.199 μs .. 22.73 μs)
variance introduced by outliers: 46% (moderately inflated)

benchmarking aeson-binary-parser/jp10
time                 513.6 μs   (500.9 μs .. 527.9 μs)
                     0.991 R²   (0.980 R² .. 0.998 R²)
mean                 509.4 μs   (500.8 μs .. 525.2 μs)
std dev              37.33 μs   (23.17 μs .. 63.47 μs)
variance introduced by outliers: 63% (severely inflated)

benchmarking aeson-binary-parser/jp100
time                 2.820 ms   (2.780 ms .. 2.857 ms)
                     0.998 R²   (0.998 R² .. 0.999 R²)
mean                 2.837 ms   (2.810 ms .. 2.863 ms)
std dev              85.33 μs   (69.77 μs .. 106.9 μs)
variance introduced by outliers: 15% (moderately inflated)

benchmarking aeson-binary-parser/jp50
time                 1.558 ms   (1.538 ms .. 1.580 ms)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 1.561 ms   (1.547 ms .. 1.581 ms)
std dev              52.65 μs   (40.79 μs .. 69.67 μs)
variance introduced by outliers: 21% (moderately inflated)

benchmarking aeson-binary-parser/numbers
time                 525.5 μs   (517.9 μs .. 531.2 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 523.6 μs   (519.4 μs .. 528.0 μs)
std dev              14.20 μs   (12.08 μs .. 17.13 μs)
variance introduced by outliers: 18% (moderately inflated)

benchmarking aeson-binary-parser/twitter1
time                 24.10 μs   (22.26 μs .. 27.23 μs)
                     0.946 R²   (0.891 R² .. 0.997 R²)
mean                 23.28 μs   (22.60 μs .. 25.68 μs)
std dev              3.560 μs   (1.285 μs .. 8.118 μs)
variance introduced by outliers: 93% (severely inflated)

benchmarking aeson-binary-parser/twitter10
time                 156.8 μs   (147.0 μs .. 170.0 μs)
                     0.946 R²   (0.916 R² .. 0.985 R²)
mean                 164.7 μs   (153.1 μs .. 184.7 μs)
std dev              47.13 μs   (30.32 μs .. 75.49 μs)
variance introduced by outliers: 98% (severely inflated)

benchmarking aeson-binary-parser/twitter100
time                 2.212 ms   (2.037 ms .. 2.418 ms)
                     0.961 R²   (0.926 R² .. 0.998 R²)
mean                 2.065 ms   (2.022 ms .. 2.167 ms)
std dev              209.8 μs   (74.13 μs .. 384.5 μs)
variance introduced by outliers: 68% (severely inflated)

benchmarking aeson-binary-parser/twitter20
time                 391.9 μs   (389.4 μs .. 394.3 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 393.7 μs   (391.1 μs .. 396.8 μs)
std dev              9.649 μs   (7.781 μs .. 12.83 μs)
variance introduced by outliers: 17% (moderately inflated)

benchmarking aeson-binary-parser/twitter50
time                 1.042 ms   (1.031 ms .. 1.052 ms)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 1.041 ms   (1.033 ms .. 1.051 ms)
std dev              30.58 μs   (25.89 μs .. 37.63 μs)
variance introduced by outliers: 19% (moderately inflated)

Benchmark criterion: FINISH
```
