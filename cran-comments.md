## Test environments
* local R installation, R 4.1.0
* ubuntu 16.04 (on travis-ci), R 4.1.0
* win-builder (devel)
* github CI:
    - {os: macOS-latest,   r: 'release'}
    - {os: windows-latest, r: 'release'}
    - {os: windows-latest, r: '3.6'}
    - {os: windows-latest, r: 'release'}
    - {os: ubuntu-18.04,   r: '4.1'}
    - {os: ubuntu-18.04,   r: '4.0'}
    - {os: ubuntu-18.04,   r: '3.6'}

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
* Fixes the broken code which caused it to get bumped from CRAN on 30.06.2021.

