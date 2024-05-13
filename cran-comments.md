## New feature for bulk download
This is an upgrade to version 1.0.0 because the suite is now a complete wrapper of all functions in the API. In this version I have:

* added `ct_get_bulk`, which allows for downloading bulk files from comtradr

## Test environments

Rhub is currently not available due to a SSL certificate problem, I have used 
github actions instead to test and the standard https://win-builder.r-project.org/ environment

* local OS X x86_64-apple-darwin23.2.0(64-bit), R 4.3.3
* Ubuntu 22.04.4 LTS-R version 4.4.0 (2024-04-24)
* Windows Server 2022, R-devel, 64 bit

## R CMD check results


There were no ERRORs or WARNINGs. 

There is 1 NOTE.

1. One related to URLs: 

```
Found the following (possibly) invalid URLs:
  URL: https://comtradeapi.un.org/files/v1/app/reference/Reporters.json
    From: man/country_codes.Rd
    Status: 404
    Message: Not Found
  URL: https://comtradeapi.un.org/files/v1/app/reference/partnerAreas.json
    From: man/country_codes.Rd
    Status: 404
    Message: Not Found
```
These URLs are online and working, I am not sure what causes this issue. 
I can not see any redirects that may cause this. This is the URL provided by the 
UN hence I cannot change it. 

----

Thanks and looking forward to the review!
Paul Bochtler
