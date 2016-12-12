library(googlesheets)
P <- rprojroot::find_package_root_file
.ed_metadata <- gs_read_csv(gs_url("https://docs.google.com/spreadsheets/d/1eHCpzYCL5-GRMZLhqJc4fj2iVUhjVhydNEp20oQW5H0/"))
readr::write_csv(.ed_metadata, P("data-raw/ed_metadata.csv"))
devtools::use_data(.ed_metadata, internal = TRUE, overwrite = TRUE)

#
# mtb1 <- map_df(1:5, function(x) {
#   a <- ed_get(endpoints[x], header_only = TRUE)
#   b <- fix_names(a)
#   data_frame(table = db_tables[x], order = seq_along(a), original_name = a, processed_name = b)
# })
#
#
# library(googlesheets)
# old_mtb <- gs_read_csv(gs_url("https://docs.google.com/spreadsheets/d/1eHCpzYCL5-GRMZLhqJc4fj2iVUhjVhydNEp20oQW5H0/"))
#
# mtb2 = mtb1 %>%
#   left_join(select(old_mtb, table, original_name, description), by=c("original_name", "table"))
#
# write_clip(mtb2)
