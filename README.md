
<!-- README.md is generated from README.Rmd. Please edit that file -->

![](inst/images/README-eidith-logo-2014.png)

# The EIDITH R Package

The **eidith** R package provides programmatic access and analytical
tools for data from the PREDICT program.

The **eidith** package contains no data. To access data via this package, you must be a
member of the PREDICT project.

The package has been updated following the end of the PREDICT project in September 2020. It now accesses a static backup of the EIDITH database, with permission enforced via Google Drive. Contact Emma if you need access.

### Package Installation
To install the package, you must have an active GitHub PAT associated with your EHA account, saved in your `.Renviron` (which can be viewed via `usethis::edit_r_environ()`). If you do not have a PAT, or if it hasn't been used in over a year [or a month?], follow the instructions here: https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token. Save your new token in your `.Renviron` as `GITHUB_PAT=[paste_your_token_here]`. Restart your R session.

You can then install the package with `remotes::intall_github("ecohealthalliance/eidith")`.

### Database download

Before downloading the database, run `googledrive::drive_auth(email = "yourname@ecohealthalliance.org")` and follow the prompts. If you are working on the server or running into any issues, try `googledrive::drive_auth(email = "yourname@ecohealthalliance.org", use_oob=TRUE)`. If you are still running into issues, reach out to Emma. 

Once your Google auth is set up, download the database with `import_local_db(database = "eha")`. If you have Malaysia or global PREDICT access, you can enter 
`"eha_with_malaysia"` or `"global"` for the `database` argument. 

### Reading in tables
 
As in earlier versions of this package, once your database is downloaded, you can use the `ed2_x()` functions to read in tables.
The following functions are available:

`ed2_animals()`,
`ed2_behavior()`,
`ed2_crop_production()`,
`ed2_dwellings()`,
`ed2_events()`,
`ed2_extractive_industry()`,
`ed2_human()`,
`ed2_human_animal_production()`,
`ed2_human_animal_production_ehp()`,
`ed2_human_crop_production()`,
`ed2_human_ehp()`,
`ed2_human_extractive_industry()`,
`ed2_human_hospital_worker()`,
`ed2_human_hunter()`,
`ed2_human_hunter_ehp()`,
`ed2_human_market()`,
`ed2_human_restaurant()`,
`ed2_human_sick_person()`,
`ed2_human_temporary_settlements()`,
`ed2_human_zoo()`,
`ed2_market_value_chain()`,
`ed2_natural_areas()`,
`ed2_specimens()`,
`ed2_test_interpreted()`,
`ed2_test_serology()`,
`ed2_tests()`,
`ed2_training()`,
`ed2_wildlife_restaurant()`

### Data processing support functions

To unnest multi-response fields, see `?ed2_expand_long` and `?ed2_expand_wide`.

### Note on deprecated functions 

`ed_db_download()` and `ed2_get()`, which previously accessed the EIDITH API, are now disabled.


