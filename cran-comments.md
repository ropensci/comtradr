## Patch for policy violation

* The package used rappdirs::user_cache_dir for determining the correct cache development. This 
has now been replaced with tools::R_user_dir('comtradr', which = 'cache'). 

* The package left a file that resulted from a faulty test in the cache directory.
This is not happening anymore. 

#### Further comments

* I understand that the previous version created some additional files that were annoying to the
CRAN team. Sorry about that. However, I would like to make a few comments that I think might 
enhance the process for future submitters. Please take these comments in the light of my relative ignorance concerning the underlying processes.

1) The documentation of `tools` is wrong. In the help page for the package it specifies `rappdirs` as a valid 
tool to specify cache directories. See: https://stat.ethz.ch/R-manual/R-patched/library/tools/html/userdir.html 
This almost certainly will lead others astray. 

--> Maybe you could use your authority as CRAN to let the R-Core Developers know that this is in fact wrong. I have already done the same for the people maintaining `rappdirs` and they have changed their documentation. 

2) I do not understand why the package was accepted previously. It adds to the mystery of CRAN that seemingly not all tests that could lead to the removal of a package are done all the time, but just sometimes. Now people have already adopted the package in a previous iteration (version 0.4.0) which also includes the "wrong" cache directory.

--> Is there a possibility to run tests that would trigger a removal **consistently** for all submissions? 

3) Would it be advisable to restrict write-permissions for users on the CRAN servers? If packages are not allowed to write to the cache directory specified by rappdirs, maybe write-permissions could be restricted so that packages error out instead of creating clutter that the CRAN team has to sweep up after. 


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
