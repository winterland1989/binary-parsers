# Revision history for binary-parsec

## 0.2.1.1  -- 2016-09-21

* Minor optimization to 'takeWhile', 'signed'.
* Add documents on backtracking.

## 0.2.1.0  -- 2016-09-21

* Add `parse`,`parseDetail`, `parseDetailLazy`.
* Reduce these running functions' overhead so that binary performs well on small getters now.
* Add scanner to benchmarks.

## 0.2.0.0  -- 2016-09-21

* Add `endOfLine` combinator.
* Add http request parsing benchmark.
* Fix a numeric parser bug.
* Fix wrong documents.
