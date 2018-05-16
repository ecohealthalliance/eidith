***************************
### Metadata New Endpoint README:

This document explains how to add a new endpoint to the R-EIDITH package, and expects that the reader 
has some familiarity with how to update the metadata googlesheet. A document detailing that process
is available at `data-raw/metadata_update_readme.md`.

#### Add API endpoint to `api_calls.R`

The first step is adding the endpoint to the `api_calls.R` script. If it makes sense to add it to a
group of tables, add it to the `p1_api_endpoints()` function, or the `p2_api_endpoints()` function.
The name chosen for each endpoint so far has been linked to the API url for ease of communication, 
for instance, the PREDICT 2 "HumanExtractiveIndustry" table's API is:

https://predict2api.eidith.org/api/Extract/ExtractHumanExtractiveIndustryData

There are some inconsistencies with the mapping of endpoint names to API urls in the PREDICT 2 
endpoint, so an added clause in the `ed_get()` or `ed2_get()` functions may be necessary if the
new endpoint does not follow one of the PREDICT 1 or PREDICT 2 patterns. 

If the new endpoint warrants a dedicated `ed_get_*()` function please follow the format provided
by the dedicated functions at the end of the `api_calls.R` script.

#### Deal with table names and IDs in `database.R`

Changes also need to be made to the `database.R` script. A table name needs to be chosen for the
given endpoint and this name should be included in either the `db_tables` or `db2_tables` vectors, or
added to a new table vector. In addition, the new table name and endpoint should be added to the 
`p1_tables_names` or `p2_table_names` lookup lists, or appended to a new list. The names of tables 
thus far have been lower case, and tables associated with PREDICT 2 have had an underscore, 
such as *animals_2*. 

In addition to setting up a table name / endpoint lookup list, the unique index for the table as well
as an other index for the table need to be added to the `db_unique_indexes` and `db_other_indexes` lists.
These lists help with creating valid SQL tables in the SQLite database. Notice that for many PREDICT 2
tables, the unique index is the created `integer_id` variable because the unique ID columns for those
tables are not actually unique in the downloaded data. If this is the case for your new endpoint, you can use
the same logic used in the `ed_db_download()` function (in the p2_table lapply section lines 169-185). 

#### Add convenience function to `tables.R`

The last step to adding a new endpoint / table to the package is to add a `ed*_*` convenience function
at the end of the `tables.R` file. Examples of this include `ed_animals()` or `ed2_animal_production()`
and allow the user to quickly pull a specific table from the local SQLite database. 

#### Change / Add metadata to appropriate sheets

If you choose to not include the group the new endpoint / table into the existing P1 or P2 lists, you'll
have to work through the downloading, status-checking, and metadata code to add your new group to the package logic.
If you've added a P1 tables, you should edit the googlesheet used to source the `ed_metadata()` function; if 
you've added a P2 table, you should edit the googlesheet used to source the `ed2_metadata()` function. For more
information about the metadata sheets, check out `data-raw/metadata_update_readme.md`.
