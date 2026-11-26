## Resubmission

This is a resubmission. The previous submission had an overall check time
of 14 min on win-builder (r-devel-windows). The slowest tests are now skipped
on CRAN, which cuts the test time by about two thirds.

## Test environments
* local ubuntu 24.04 install, R 4.6.2
* ubuntu-latest (on GitHub actions), R-devel (4.7.0), R 4.6.2, R 4.5.3
* macOS-latest (on GitHub actions), R 4.6.2
* windows-latest (on GitHub actions), R 4.6.2
* win-builder, R-devel, R-release, R-oldrelease

## R CMD check results

0 errors | 0 warnings | 0 notes

This release fixes the test failures on the CRAN check results for 1.1.1
(`tidyr::gather()` on mixtime columns, now `tidyr::pivot_longer()`). It also
requires mixtime (>= 0.5.1), since binaries of mixtime 0.5.0 fail with
vecvec 1.5.0.

## Revdep checks

All 8 reverse dependencies (fable.bayesRecon, fable.intermittent, fabletools,
feasts, fpp3, icomb, iNZightTS, mixtime) were checked against the CRAN and
development versions of ggtime. All pass R CMD check with both, so none have
changed to worse.
