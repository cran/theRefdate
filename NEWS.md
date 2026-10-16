# theRefdate 1.1.0

## New: date format ("flavour") can be chosen once per session

* Until now the package worked on strings in the format `"DD.MM.YYYY"` only.
  It now supports three formats, called *flavours*:
  * `"dmy"`: `"DD.MM.YYYY"` (default, identical to version 1.0.0)
  * `"iso"`: `"YYYY-MM-DD"`
  * `"iso_basic"`: `"YYYYMMDD"`
* New function `setFlavour()` selects the flavour for the whole R session.
  All functions then work on strings of that format. Calling `setFlavour()`
  again with a *different* flavour in the same session gives a warning,
  because two formats in one session usually indicate a mistake.
  The old identifiers `"LP"` (= `"dmy"`) and `"VK"` (= `"iso"`) are still
  accepted.
* `flavour()` returns the active flavour. It now returns `"dmy"` by default
  (version 1.0.0 returned `"LP"`).
* `jahr()`, `monat()`, `tag()`, `dateDiff()` and `dateAddMonths()` have a new
  optional argument `format` that overrides the session flavour for a single
  call.

## Unchanged: results without `setFlavour()`

If `setFlavour()` is never called, the package works in the format
`"DD.MM.YYYY"` and returns the same results as version 1.0.0 for every valid
input (day, month and year in the format, years 1000 to 9998, whole months).
A test (`tests/testthat/test-kompatibel-1.0.0.R`) compares the results with
values produced by the version published on CRAN.

## Changed: errors instead of `NA` and instead of nonsense results

Version 1.0.0 answered invalid input with `NA` or with a malformed string.
Version 1.1.0 stops with an error message that names the argument, its class
and value, and the expected format.

* **`NA` is now an error.** `jahr()`, `monat()`, `tag()`, `dateDiff()` and
  `dateAddMonths()` no longer return `NA` for an `NA`, an empty string or a
  string that is not a date; they signal an error. Code that relied on
  `NA` being passed through (for example `sapply()` over a column with gaps)
  needs to filter those values first or wrap the call in `tryCatch()`.
  The same applies to a call without arguments.
* A string in a different format than the active flavour (for example an ISO
  date while the default is `"DD.MM.YYYY"`) is an error.
* `dateAddMonths()` accepts whole months only (a number of length 1). A
  fractional number of months, which gave results like `"01.01.5.2000"`, is an
  error.
* `dateAddMonths()` stops if the resulting year is outside 0 to 9999. Version
  1.0.0 returned values such as `"31.01.1000"` for a result beyond the year 9999.
* The months are now added in a single step instead of recursively. Results
  for valid input are the same.

## Changed: output format of `dateAddMonths()`

* The year in the result is always written with four digits, month and day
  with two digits (`"01.01.0001"` instead of `"01.01.1"`). For years from 1000
  on nothing changes.

## Documentation

* The vignette explains the flavours and `setFlavour()`.
* Documentation of all functions describes the argument `format`.
* The help pages are now written in English (they were in German); the
  `Language` field in `DESCRIPTION` is `en-US`. Error messages are unchanged.

# theRefdate 1.0.0

* First CRAN release: date manipulation on character strings in the format
  `"DD.MM.YYYY"` with `jahr()`, `monat()`, `tag()`, `dateDiff()`,
  `dateAddMonths()` and `flavour()`.
