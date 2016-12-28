When updating to a new version of the eidith package (or downgrading), be sure to run
`ed_db_download()` afterwards to update your databse.

You can install previous versions of eidith from GitHub using the **devtools**
package, like so:

```
devtools::install_github('ecohealthalliance/eidith@v0.1.0')`
```
# eidith 0.3.0

## Misc

- Fix typos in vignette

# eidith 0.2.1 (patch)

- Fix bug in `ed_table_` when parsing long expressions, showing up in
  `ed_tests_reprort()`

# eidith 0.2.0

## New functionality

- Add a FASTA export function: `ed_fasta()`
- Add a function for reports on recents tests: `ed_tests_report()`

## Changes to data processing

 - New column in tests table - `diag_lab_shortname` - an abbreviated lab name for easier reporting
 - Renamed `known_human_risk` to `evidence_human_infection` and added more clarifying text to metadata
 - Taxagroups are now one-to-one mapped to orders. `ed_taxagroups()` will return a table with the
   mapping between them.  Metadata notes updated to reflect this. (See [Issue #32](https://github.com/ecohealthalliance/eidith/issues/32))
 
## Bug Fixes

 - No longer fail when database extracts are zero-length
 
## Changes to development

 - Moved to separate stable (`master`) and development (`dev`) branches. Stable
   branch can now be installed without devtools.
 - Set up continuous integration tests, run daily, for all main OSs and R versions. These will tell us if patches
   or new data break something in the package.
 
# eidith v0.1.0

* Add documentation of web API

# eidith v0.0.1

* Beta release
* Database download, management, and data extraction functions in stable
  form and documented

# eidith v0.0.0.9000

* Initial development version



