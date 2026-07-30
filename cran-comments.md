## Replacing the poorman dependency ahead of its archival

* This release replaces the `poorman` dependency with `dplyr`. CRAN has
  scheduled `poorman` for archival on 2026-08-21; this release removes the
  dependency well before that date. The swap does not add any new recursive
  dependencies, as all of `dplyr`'s dependencies were already required
  through other imports.
* Additionally, three small bug fixes (see NEWS.md): validation of the
  `frequency` argument in bulk requests, clearer error messages for invalid
  partner codes, and a fixed error path in `ct_migrate_cache()`.

## Test environments

* local: Ubuntu (linux/aarch64), R 4.5.3
* win-builder: R Under development (2026-07-29 r90317 ucrt),
  Windows Server 2022 x64
* R-hub: linux (R-devel), macos (R-devel), windows (R-devel) via GitHub
  Actions
* GitHub Actions CI: ubuntu-latest (release, devel, oldrel-1),
  windows-latest (release, oldrel-4), macos-latest (release)

## R CMD check results

There were no ERRORs, WARNINGs or NOTEs.


----

Thanks and looking forward to the review!
Paul Bochtler
