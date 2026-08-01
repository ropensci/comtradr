## New feature: the UN Comtrade estimated trade matrix

* This release adds `ct_get_trade_matrix()`, which queries the UN Comtrade
  estimated trade matrix endpoint. Unlike the main database, this analytical
  product complements reported figures with UN estimates, so the returned
  reporter x partner matrix also covers countries that filed nothing for a
  given year.
* The function is marked `lifecycle::badge("experimental")`: the endpoint is
  not part of the UN Comtrade public API documentation, so its field
  semantics may change without notice.
* A new vignette, `trade_matrix`, documents the endpoint's two main pitfalls
  (aggregate "World" rows that cause fourfold over-counting when summed, and
  the per-cell meaning of the `is_reported` flag). It is pre-computed and
  builds without network access.
* `ct_get_data()` now aborts with a pointer to the new function when called
  with `commodity_classification = "TM"`. This value was already rejected in
  1.0.6, so no previously working call changes behaviour; only the error
  message and condition class differ.

## Test environments

* local: Ubuntu (linux/aarch64), R 4.5.3
* win-builder: R Under development, Windows Server 2022 x64
* R-hub: linux (R-devel), macos (R-devel), windows (R-devel) via GitHub
  Actions
* GitHub Actions CI: ubuntu-latest (release, devel, oldrel-1),
  windows-latest (release, oldrel-4), macos-latest (release)

## R CMD check results

There were no ERRORs, WARNINGs or NOTEs.

## Notes

* The package ships a small pre-computed slice of API responses for the new
  vignette (~1.7 KB) and a recorded HTTP fixture for tests. UN Comtrade's
  re-dissemination policy explicitly permits free-of-charge extracts of up to
  100,000 records and does not copyright aggregated (transformed) data.
* All tests and the vignette run without an API key; network-dependent tests
  skip cleanly.


----

Thanks and looking forward to the review!
Paul Bochtler
