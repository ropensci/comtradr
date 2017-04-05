## Test environments
* local Windows 7 install, R 3.3.3
* local OS X install, R 3.3.3
* ubuntu 12.04 (on travis-ci), R 3.3.2
* CRAN win-builder, R 3.4.0

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:

* Possibly mis-spelled words in DESCRIPTION:
  - API (2:55, 6:12)
  - Comtrade (2:46, 6:3)

  These are words that are not in the dictionary and are also not misspelled.

## Downstream dependencies
None

## Comments/Disclosures
* This is my first time submitting this package to CRAN.
* The UN Comtrade website has an R tutorial, basically some examples of interacting with their API using R code, this can be found here, https://comtrade.un.org/data/Doc/api/ex/r. The purpose of this package is to improve upon the code they've posted, and add features that allow the end user to interact with the API through R in a more complete way (for example, without having to go to the Comtrade website to look up HS commodity codes). If you'd like a thorough breakdown of the features added and improvements made between their code examples and the functions in this package, I would be happy to provide that. The UN Comtrade R tutorial is linked to in the README and is listed as an inspiration for this package.
