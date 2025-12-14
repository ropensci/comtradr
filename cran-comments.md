## Update with more metadata access

* this includes an update to return all metadata tables

## Test environments

 1 🖥  linux          R-* (any version)                     ubuntu-latest on GitHub
 2 🖥  m1-san         R-* (any version)                     macos-15 on GitHub, ASAN + UBSAN on macOS
 4 🖥  macos-arm64    R-* (any version)                     macos-latest on GitHub
 5 🖥  windows        R-* (any version)                     windows-latest on GitHub
 6 🐋 atlas          R-devel (2025-12-09 r89129)           Fedora Linux 38 (Container Image)
 7 🐋 c23            R-devel (2025-12-08 r89124)           Ubuntu 22.04.5 LTS
 8 🐋 clang-asan     R-devel (2025-12-09 r89129)           Ubuntu 22.04.5 LTS
 9 🐋 clang-ubsan    R-devel (2025-12-09 r89129)           Ubuntu 22.04.5 LTS
10 🐋 clang16        R-devel (2025-12-08 r89124)           Ubuntu 22.04.5 LTS
11 🐋 clang17        R-devel (2025-12-08 r89124)           Ubuntu 22.04.5 LTS
12 🐋 clang18        R-devel (2025-12-08 r89124)           Ubuntu 22.04.5 LTS
13 🐋 clang19        R-devel (2025-12-08 r89124)           Ubuntu 22.04.5 LTS
15 🐋 donttest       R-devel (2025-12-08 r89124)           Ubuntu 22.04.5 LTS
16 🐋 gcc-asan       R-devel (2025-12-09 r89129)           Fedora Linux 40 (Container Image)
17 🐋 gcc13          R-devel (2025-12-09 r89129)           Fedora Linux 38 (Container Image)
18 🐋 gcc14          R-devel (2025-12-09 r89129)           Fedora Linux 40 (Container Image)
19 🐋 gcc15          R-devel (2025-12-09 r89129)           Fedora Linux 42 (Container Image)
20 🐋 intel          R-devel (2025-12-09 r89129)           Fedora Linux 38 (Container Image)
21 🐋 lto            R-4.5.2 (2025-10-31)                  Ubuntu 24.04.3 LTS
22 🐋 mkl            R-devel (2025-12-09 r89129)           Fedora Linux 38 (Container Image)
23 🐋 nold           R-devel (2025-12-09 r89129)           Ubuntu 22.04.5 LTS
24 🐋 noremap        R-devel (2025-12-08 r89124)           Ubuntu 22.04.5 LTS
27 🐋 ubuntu-clang   R-devel (2025-12-09 r89129)           Ubuntu 22.04.5 LTS
28 🐋 ubuntu-gcc12   R-devel (2025-12-09 r89129)           Ubuntu 22.04.5 LTS
29 🐋 ubuntu-next    R-4.5.2 (patched) (2025-12-06 r89124) Ubuntu 24.04.3 LTS
30 🐋 ubuntu-release R-4.5.2 (2025-10-31)                  Ubuntu 24.04.3 LTS

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
