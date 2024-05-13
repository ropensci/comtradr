## New feature for bulk download
This is an upgrade to version 1.0.0 because the suite is now a complete wrapper of all functions in the API. In this version I have:

* added `ct_get_bulk`, which allows for downloading bulk files from comtradr

## Test environments

* local OS X x86_64-apple-darwin23.2.0(64-bit), R 4.3.3
*  1 🖥  linux          R-* (any version)                     ubuntu-latest on GitHub
*  2 🖥  macos          R-* (any version)                     macos-13 on GitHub
*  3 🖥  macos-arm64    R-* (any version)                     macos-latest on GitHub
*  4 🖥  windows        R-* (any version)                     windows-latest on GitHub
*  5 🐋 atlas          R-devel (2024-05-12 r86534)           Fedora Linux 38 (Container Image)
*  6 🐋 clang-asan     R-devel (2024-05-12 r86534)           Ubuntu 22.04.4 LTS
*  7 🐋 clang16        R-devel (2024-05-11 r86532)           Ubuntu 22.04.4 LTS
*  8 🐋 clang17        R-devel (2024-05-11 r86532)           Ubuntu 22.04.4 LTS
*  9 🐋 clang18        R-devel (2024-05-11 r86532)           Ubuntu 22.04.4 LTS
* 10 🐋 donttest       R-devel (2024-05-11 r86532)           Ubuntu 22.04.4 LTS
* 11 🐋 gcc13          R-devel (2024-05-12 r86534)           Fedora Linux 38 (Container Image)
* 12 🐋 intel          R-devel (2024-05-12 r86534)           Fedora Linux 38 (Container Image)
* 13 🐋 mkl            R-devel (2024-05-12 r86534)           Fedora Linux 38 (Container Image)
* 14 🐋 nold           R-devel (2024-05-12 r86534)           Ubuntu 22.04.4 LTS
* 16 🐋 ubuntu-clang   R-devel (2024-05-12 r86534)           Ubuntu 22.04.4 LTS
* 17 🐋 ubuntu-gcc12   R-devel (2024-05-12 r86534)           Ubuntu 22.04.4 LTS
* 18 🐋 ubuntu-next    R-4.4.0 (patched) (2024-05-12 r86534) Ubuntu 22.04.4 LTS
* 19 🐋 ubuntu-release R-4.4.0 (2024-04-24)                  Ubuntu 22.04.4 LTS
* 20 🐋 valgrind       R-devel (2024-05-12 r86534)           Fedora Linux 38 (Container Image)
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
