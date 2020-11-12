
<!-- README.md is generated from README.Rmd. Please edit that file -->

![](inst/images/README-eidith-logo-2014.png)

# The EIDITH R Package

The **eidith** R package provides programmatic access and analytical
tools for data from the PREDICT program.

The **eidith** package contains no data. To access data via this package, you must be a
member of the PREDICT project.

The package has been updated since the end of the PREDICT project in September 2020 to access static backups of the EIDITH database.

To download the database, run `import_local_db(database = "eha")`. If you have Malaysia or global PREDICT access, you can enter 
`"eha_with_malaysia"` or `"global"` for the `database` argument. Access to the database is enforced via Google Drive--contact Emma (mendelsohn@ecohealthalliance.org) with questions.

When downloading the database, you will be prompted in your R console to allow the `googledrive` package to access to your Google account. 

As in earlier versions of this package, once your database is downloaded, you can use the `ed2_x()` functions to read in tables (`ed2_events()`, `ed2_animals()`, etc.)

Note the functions `ed_db_download()` and `ed2_get()`, which previously accessed the EIDITH API, are now disabled.


