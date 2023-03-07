## Resubmission

This is a resubmission. I fixed an issue that I had not caught in my
pre-submission tests. I was able to replicate the error found by CRAN's
incoming checks on Win-builder R-devel, and confirmed that the fixed version
does not have the same error by uploading to Win-builder. The error was related
to a string being incorrectly interpreted as a filename.

## Test environments
* local Mac OS 13.2.1, R 4.2.2
* Ubuntu 20.04.1 LTS, R-release, GCC (via R-hub)
* Windows Server 2022, R-devel (2023-03-06 r83941 ucrt) (via win-builder)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

There are no reverse dependencies.
