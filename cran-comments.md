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
(`tidyr::gather()` on mixtime columns, now `tidyr::pivot_longer()`).

## Revdep checks

All reverse dependencies have been checked, none have changed to worse.
