comtradr 0.0.2.9000
===================

### BUG FIXES

* commodity_lookup() was returning a named vector in specific situations (see [issue #2](https://github.com/ChrisMuir/comtradr/issues/2)). This issue is now fixed. Setting arg `return_char` to TRUE should always return a (non-named) vector, while FALSE should always return a list.


comtradr 0.0.2
===================

### NEW FEATURES

* commodity_lookup(): Expanded function to accept multiple commodities or commodity codes (as either character vector or numeric vector). Also added argument "return_char" that allows the user to specify list output or char vector output, and argument "return_code" that specifies output of commodity descriptions or commodity codes.

### MINOR IMPROVEMENTS

* Add unit tests via [testthat](https://github.com/hadley/testthat).


comtradr 0.0.1 (2017-04-06)
===========================

### NEW FEATURES

* released to CRAN
