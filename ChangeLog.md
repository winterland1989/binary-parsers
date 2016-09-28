# Revision history for binary-parsec

## 0.2.3.0  -- 2016-09-28

* Remove `MultiWayIf` to support older GHC (7.4 and 7.6).
* Add `eitherDecoder` and `maybeDecoder`.


## 0.2.2.0  -- 2016-09-21

* Minor optimization to 'takeTill', 'takeWhile' and 'signed'.
* Add documents on backtracking.

## 0.2.1.0

* Add `parse`,`parseDetail`, `parseDetailLazy`.
* Reduce these running functions' overhead so that binary performs well on small getters now.
* Add scanner to benchmarks.

## 0.2.0.0

* Add `endOfLine` combinator.
* Add http request parsing benchmark.
* Fix a numeric parser bug.
* Fix wrong documents.
