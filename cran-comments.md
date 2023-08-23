## Test environments

* local OS X install, R 4.1.1
* Ubuntu Linux 20.04.1 LTS, R-release, GCC (Rhub)
* Fedora Linux, R-devel, clang, gfortran (Rhub)
* Windows (devel and release)

## R CMD check results

This is a re-release. The Comtrade API has been rewritten entirely. This re-release reflects these changes. Package had been archived before at the request of previous maintainer. 

There were no ERRORs or WARNINGs. 

There are 4 NOTES.


1. One related to the archival of this package:
```
* checking CRAN incoming feasibility ... [4s/25s] NOTE
Maintainer: ‘Paul Bochtler <paulbochtler.gh@gmail.com>’

New submission

Package was archived on CRAN

Possibly misspelled words in DESCRIPTION:
  Comtrade (2:42, 26:70, 27:39)

CRAN repository db overrides:
  X-CRAN-Comment: Archived on 2023-04-12 at the request of the
    maintainer.

```

2. Two that are only found on Windows (Server 2022, R-devel 64-bit): 

```
* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'
```
As noted in [R-hub issue #503](https://github.com/r-hub/rhub/issues/503), this could be due to a bug/crash in MiKTeX and can likely be ignored.

3. The third is 

```
* checking for non-standard things in the check directory ... NOTE
Found the following files/directories:
  ''NULL''
```

As noted in [R-hub issue #560](https://github.com/r-hub/rhub/issues/560), this seems to be an Rhub issue and so can likely be ignored. 

4. A fourth that is found with *Fedora Linux, R-devel, clang, gfortran* and *Ubuntu Linux 20.04.1 LTS, R-release, GCC*

```
* checking HTML version of manual ... NOTE
Skipping checking HTML validation: no command 'tidy' found
```

This also seems to be a recurring issue on Rhub [R-hub issue #560](https://github.com/r-hub/rhub/issues/548) and so can likely be ignored.




Thanks!
Paul Bochtler
