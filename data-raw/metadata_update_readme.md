***************************
Metadata Management README:

The EIDITH R package automatically updates PREDICT-2 metadata via a googlesheets query
whenever an `ed2_metadata()` call is made. This occurs when downloading tables, when
tables are validated through a status check, and when a user explicitly uses `ed2_metadata()`
to return a dataframe of PREDICT-2 metadata.

If an internet connection is not available, then the `ed2_metadata()` call will return the
metadata file stored in the package. Depending on how long it has been since the master
branch metadata file has been updated, this file may be significantly out of date.

The ed2_metadata google spreadsheet is available at
https://docs.google.com/spreadsheets/d/1bJwPYMUaUQ7DbL9mQS55gzL-_03cIqXjq2oDyCnjVJw/edit#gid=983692865
and requires an EHA email login to access.

If a new field is added to a PREDICT-2 table, it needs to be added to the spreadsheet or else it
will not be recognized on EIDITH database download and will be dropped. This is as easy as
finding the appropriate table (eg. "Events" or "HumanSickPerson") and adding a new row with the
new field information. To keep things neat, it makes sense to keep the metadata spreadsheet
grouped by table; however, the actual location of a row doesn't matter.

Once a change is made, the spreadsheet will automatically save and generate a csv file which
gets pulled from the `ed2_metadata()` call. There is no need to do anything but edit the
google spreadsheet in order to change existing metadata or add a new metadata field. Below are
descriptions of each column in the metadata spreadsheet:

***************************
Spreadsheet Column Descriptions:

The `table` column in the spreadsheet requires all lower-case letters with a trailing "_2" to
indicate that it is a PREDICT 2 table.

The `endpoint2` column must be one of the p2 endpoints (a list of these endpoints can be found
with a call of the `eidith::p2_api_endpoints()` function).

The `order` column is used to order columns for the user, and isn't necessary unless there is
a reason why a new

The `original_name` column reflects the exact case-sensitive and space-sensitive name of each
column as it appears when downloaded raw from the EIDITH API. Sometimes there are misspellings,
added spaces, or unusual names, so it's good to be careful here.

The `auto_processed_name` column is just the lower_snake_case version of the `original_name`
column. The value in this column will get used as the field name, unless there is an entry in
the `replacement_name` column.

The `replacement_name` column is used to override an `auto_processed_name` as the name given
to a field in case the `auto_processed_name` is misspelled or misleading. The vast majority of
`replacement_name` rows are blank, because the `auto_processed_name` is just fine. In general,
we try to mostly stick with the `auto_processed_name` to ease communication, since users may be
used to these names from the eidith.org website or other sources. It makes sense to change
field names when there are spelling errors, they are truly misleading, or if they are identical
to field names from other tables that might be joined together. In the first two cases, it is
good to let the IM team at Metabiota know that there is a misspelling or a very confusing name.

The `description` field is for the metadata description of the field. Please use the actual
wording of a question asked, or the description of a field from another form of documentation.
It is better practice to leave the description blank or put a ?? to indicate that the exact
definition of the field isn't known to the R package team than to make something up which
might confuse or mislead users.

The `processing_notes` column is used for notes in processing, and in many cases mentions if
the field is semi-colon separated or is a categorical variable with a certain number of
categories.

The `question` column is specifically for questionnaire data and identifies exactly which
question this field refers to, e.g. (Q8 or Q17).

***************************
