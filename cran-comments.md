## Test environments
* local R installation, R 4.0.2
* ubuntu 16.04 (on travis-ci), R 4.0.2
* win-builder (devel)
* Running on GitHub Actions:
    * {os: macOS-latest,   r: 'release'}
    * {os: windows-latest, r: 'release'}
    * {os: windows-latest, r: '3.6'}
    * {os: windows-latest, r: 'release'}
    * {os: ubuntu-16.04,   r: 'release'}
    * {os: ubuntu-16.04,   r: 'oldrel'}

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
* 1 Note is for current time verification not working

## By CRAN request:
- adapted the description in DESCRIPTION to no include back-ticks, and added publication reference  
- removed zero leading minor version number  
- added more return values for documentation  
- omitted doi space in DESCRIPTION  
- quoted 'ggplot2' in DESCRIPTION  
- no tests of vignetted should write anywhere  
- only possible paths found in dontrun examples for read_freesurfer functions, where the example shows how to use the 'freesurfer' package to find the data directory of this software  
