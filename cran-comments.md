## Patch for policy violation

* this is a patch to fix a bug that was introduced when fixing the bug about tidy cols. Now tidy cols are returned again. 
* testing for this error was introduced in the test environment to avoid this in the future.

## Test environments

 1 🖥  linux          R-* (any version)                     ubuntu-latest on GitHub
 2 🖥  macos          R-* (any version)                     macos-13 on GitHub
 3 🖥  macos-arm64    R-* (any version)                     macos-latest on GitHub
 4 🖥  windows        R-* (any version)                     windows-latest on GitHub
 5 🐋 atlas          R-devel (2024-11-14 r87333)           Fedora Linux 38 (Container Image)
 6 🐋 c23            R-devel (2024-11-13 r87330)           Ubuntu 22.04.5 LTS
 7 🐋 clang-asan     R-devel (2024-11-14 r87333)           Ubuntu 22.04.5 LTS
 8 🐋 clang16        R-devel (2024-11-13 r87330)           Ubuntu 22.04.5 LTS
 9 🐋 clang17        R-devel (2024-11-13 r87330)           Ubuntu 22.04.5 LTS
10 🐋 clang18        R-devel (2024-11-13 r87330)           Ubuntu 22.04.5 LTS
11 🐋 clang19        R-devel (2024-11-13 r87330)           Ubuntu 22.04.5 LTS
12 🐋 clang20        R-devel (2024-10-09 r87215)           Ubuntu 22.04.5 LTS
13 🐋 donttest       R-devel (2024-11-13 r87330)           Ubuntu 22.04.5 LTS
14 🐋 gcc13          R-devel (2024-11-14 r87333)           Fedora Linux 38 (Container Image)
15 🐋 gcc14          R-devel (2024-11-14 r87333)           Fedora Linux 40 (Container Image)
16 🐋 intel          R-devel (2024-11-14 r87333)           Fedora Linux 38 (Container Image)
17 🐋 mkl            R-devel (2024-11-14 r87333)           Fedora Linux 38 (Container Image)
18 🐋 nold           R-devel (2024-11-14 r87333)           Ubuntu 22.04.5 LTS
19 🐋 noremap        R-devel (2024-11-13 r87330)           Ubuntu 22.04.5 LTS
22 🐋 ubuntu-clang   R-devel (2024-11-14 r87333)           Ubuntu 22.04.5 LTS
23 🐋 ubuntu-gcc12   R-devel (2024-11-14 r87333)           Ubuntu 22.04.5 LTS
24 🐋 ubuntu-next    R-4.4.2 (patched) (2024-11-13 r87333) Ubuntu 22.04.5 LTS
25 🐋 ubuntu-release R-4.4.2 (2024-10-31)                  Ubuntu 22.04.5 LTS
26 🐋 valgrind       R-devel (2024-11-14 r87333)           Fedora Linux 38 (Container Image)

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
