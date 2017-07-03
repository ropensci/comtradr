## Resubmission
This is a resubmission. This package was accepted by CRAN on 2017-04-05, however on 2017-05-29 I asked CRAN to archive the package, via this email:

>
Hello, I'm the author and maintainer of the package comtradr on CRAN: 
https://CRAN.R-project.org/package=comtradr
>
I'm writing today because I realized yesterday that most of the core functions in my package are currently not working on Windows machines, due to a curl error. The error is:
>
```
Error in curl::curl_fetch_memory(url, handle = handle) : 
 Peer certificate cannot be authenticated with given CA certificates
```
>
Which basically means curl cannot verify the ssl certificate of the API site. After looking into it more today, I believe there actually are issues with the ssl certificates of the United Nations Comtrade website, per ssldecoder:
https://ssldecoder.org/?host=comtrade.un.org&port=&csr=&s=
>
I'm working on testing and assessing exactly what's going on and options for fixing the issues, and I'm also reaching out to the admins of the UN Comtrade website to ask about their certifications. In the meantime, it may be best to take the package down from CRAN.
>
Let me know the policies on this and your thoughts. If you have any questions for me please don't hesitate to ask, thank you.

As of a week ago, the curl issues have gone away (I believe the UN Comtrade website finally got new SSL certificates), and all package functions are now working properly. In addition, I've made the following changes to the package since v0.0.1:

* commodity_lookup(): Expanded function to accept multiple commodities or commodity codes (as either character vector or numeric vector). Also added argument "return_char" that allows the user to specify list output or char vector output, and argument "return_code" that specifies output of commodity descriptions or commodity codes.
* Removed unnecessary zzz.R file.


## Test environments
* local Windows 7 install, R 3.4.1
* local OS X install, R 3.4.0
* ubuntu 12.04.5 (on travis-ci), R 3.4.0
* CRAN win-builder, R Under development (unstable) (2017-07-01 r72871)

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:

* Possibly mis-spelled words in DESCRIPTION:
  API (2:51, 5:79, 5:223)
  Comtrade (2:42, 5:70, 5:116)

  These are words that are not in the dictionary and are also not misspelled.

## Downstream dependencies
None

## Comments/Disclosures
* The UN Comtrade website has an R tutorial, basically some examples of interacting with their API using R code, this can be found here, https://comtrade.un.org/data/Doc/api/ex/r. The purpose of this package is to improve upon the code they've posted, and add features that allow the end user to interact with the API through R in a more complete way (for example, without having to go to the Comtrade website to look up HS commodity codes). If you'd like a thorough breakdown of the features added and improvements made between their code examples and the functions in this package, I would be happy to provide that. The UN Comtrade R tutorial is linked to in the README and is listed as an inspiration for this package.
