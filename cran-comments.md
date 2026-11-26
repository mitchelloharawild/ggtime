## Test environments
* local ubuntu 24.04 install, R 4.6.2
* ubuntu-latest (on GitHub actions), R-devel (4.7.0), R 4.6.2, R 4.5.3
* macOS-latest (on GitHub actions), R 4.6.2
* windows-latest (on GitHub actions), R 4.6.2
* win-builder, R-devel, R-release, R-oldrelease

## R CMD check results

0 errors | 0 warnings | 0 notes

The 'version contains large components' note seen on the development version
is resolved by the release version bump.

This release fixes the test failures on the CRAN check results for 1.1.1
(`tidyr::gather()` on mixtime columns, now `tidyr::pivot_longer()`). It also
requires mixtime (>= 0.5.1), since binaries of mixtime 0.5.0 fail with
vecvec 1.5.0.

## Revdep checks

All 8 reverse dependencies (fable.bayesRecon, fable.intermittent, fabletools,
feasts, fpp3, icomb, iNZightTS, mixtime) were checked against the CRAN and
development versions of ggtime. All pass R CMD check with both, so none have
changed to worse.
