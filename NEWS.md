When updating to a new version of the eidith package, be sure to run
`ed_db_download()` after.

You can install previous versions of eidith from GitHub, e.g.,
`devtools::install_github('ecohealthalliance/eidith@v0.1.0')`

# eidith 0.2.0

## New functionality

- Add a FASTA export function: `ed_fasta()`
- Add a function for reports on recents test: `ed_tests_report()`

## Changes to data processing

 - New column in tests table - `diag_lab_shortname` - an abbreviated lab name for easier reporting
 - Renamed `known_human_risk` to `evidence_human_infection` and added more clarifying text to metadata
 
## Changes to development

 - Moved to separate stable (`master`) and development (`dev`) branches.
 - Set up continuous integration tests, run daily, for all main OSs and R versions. 
 
# eidith v0.1.0

* Add documentation of web API

# eidith v0.0.1

* Beta release
* Database download, management, and data extraction functions in stable
  form and documented

# eidith v0.0.0.9000

* Initial development version



