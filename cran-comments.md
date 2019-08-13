## Test environments
* local Mac OS 10.14.6, R 3.5.2
* ubuntu 14.04 (on travis-ci), R 3.6.1
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 notes

The note:
- Missing or unexported object: ‘tidyr::one_of’

That piece of code was included for forward compatibility with the upcoming release of `tidyr`, and only runs if the installed version of `tidyr` is at least v1.0

## Reverse dependencies

There are no reverse dependencies.
