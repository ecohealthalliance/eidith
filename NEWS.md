When updating to a new version of the eidith package (or downgrading), be sure to run
`ed_db_download()` afterwards to update your databse.

You can install previous versions of eidith from GitHub using the **devtools**
package, like so:

```
devtools::install_github('ecohealthalliance/eidith@v0.1.0')`
```

# eidith 0.4.1

- Two new data endpoints/tables available: `ed2_behavioral()`, which lists
  qualitative interview events, and `ed2_training()` which lists training events.
  There are primarily for reporting purposes.
- Fix warnings due to deprecated functions in the **glue** package.
- New developer documentation in the main GitHub repo for adding new endpoints
  and metadata.
- New author: Emma Mendehlson joins the team! 🎉

 # New functionality

# eidith 0.4

## New functionality

- PREDICT-2 database now available for download using `ed_db_download()` which takes
  two arguments: one for PREDICT-1 tables, the second for PREDICT-2 tables. `p1_api_endpoints()` 
  and `p2_api_endpoints()` are functions that list possible table endpoints. Errors in
  one table download will not affect others.
- PREDICT-2 local database tables available via `ed2_*()` functions (e.g. `ed2_animals()`). On 
  pull of these tables from local database, messages print to user about note columns or 
  duplicate IDs.
- New status banner details database status at startup, is also available via `ed_db_detailed_status()`
  function. Short database status checking function `ed_db_check_status()` checks each table
  then prompts downloads when run interactively.
- EIDITH field metadata available via `ed2_metadata()` function and searchable `?ed2_metadata` 
  help file. Metadata is now automatically updated on database download.
- EIDITH PREDICT-2 table connections available via `ed2_tables_conn()` function.
- Delete local EIDITH database using `ed_db_delete()` function.
- Helper functions for expanding multiple-response fields: `ed2_expand_wide()` and 
  `ed2_expand_long()`. 

## New documentation

- PREDICT-2 EIDITH database structure and joining vignette
- PREDICT-2 Multiple-response field helper function vignette
- PREDICT-2 EIDITH-R workshop presentation slides
- Updated help files for old and new functions

## *Breaking changes*

- New database functions and structure may result in errors for users with
  pre-existing PREDICT-1 local databases. It is best to install new package
  version, then run `ed_db_delete()` and run a clean `ed_db_download()` call.

# eidith 0.3.1 (patch)

- Internal changes to be compatible with dplyr 0.7.0 ([#55](https://github.com/ecohealthalliance/eidith/pull/55))

# eidith 0.3

## New functionality

- Additional functionality for FASTA export and interpretation: `ed_fasta_group()`,
  `ed_report_excel()`.
- Mock data sets now available: `ed_table_mock()`
- EIDITH table connections now listed in dataframe via `ed_tables_conn()`
- Database downloads more robust to errors, only overwrites old data after new data is checked

## New documentation

- Automatic Processing / Raw Data vignette
- Data Structure and Table Joining vignette with mock data example
- Report generation and FASTA file vignette

## *Breaking changes in data processing*

- New `human_health` field in viruses flags human health risk, old `evidence_human_infection` field is removed.
- Reordering metadata to make more intelligible.
- Change `virus_id` to `sequence_id`.
- Make lat/long numeric and compatible with `SpatialPointsDataFrame`
- Altering `date_created`, `date_modified`, and `database_date` fields to reflect tables, e.g `date_created_animals` or `database_date_specimens`.
- The `sequence` field in `tests` table has been renamed `test_sequences` to avoid confusion
- The `interpretation` field in `tests` table has been renamed `test_interpretations` to avoid confusion
- The `specimen_type` field in `tests` tables has been renamed `specimen_type_test` and the `speciment_type_group` has been renamed `specimen_type_group_test` to avoid confusion.

__Note: A new local database download (`ed_db_download`) will be needed for the user to see these data processing changes. The change in field names may break previous scripts, the user should consider saving current state of their local database using `ed_db_export()`.__



# eidith 0.2.2 (patch)

- Switch API endpoints to new URLs

# eidith 0.2.1 (patch)

- Fix bug in `ed_table_` when parsing long expressions, showing up in
  `ed_tests_report()`

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


