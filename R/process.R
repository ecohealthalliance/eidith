#' Standard post-processing of EIDITH exports
#'
#' This function takes raw data downloaded from EIDITH and puts it through
#' various preprocessing and cleaning steps.  In general there is no need to
#' call this function directly - it is called by both [download_db()] and the [direct download functions][ed_get()].
#'
#' Steps taken to clean the data include:
#'
#' -   Converting variable names from `camelCase` to `snake_case` to make it easy
#' to distinguish between raw and cleaned data.
#' -   Converting some variable names to clearer ones:  all `_id` variables are
#' numeric primary keys, other identifiers now go by `_id_name`.
#' -   Cleaning up whitespace and capitalization variability
#' -   Re-arranging table order to put the most pertinent information first.
#' -   Normalizing all animal taxonomic information to match the [ITIS](https://www.itis.gov/) database.
#' -   Coercing some free-form entries (e.g. `specimen_type`) to a standard set of categories
#' -   Converting yes/no fields to TRUE/FALSE
#' -   Fixing spelling errors
#' -   Extracting common TRUE/FALSE variables from free-form text of viral interpretation (Genbank numbers, whether virus is known, whether virus is known to affect humans).
#'
#' @param dat The dat as exported from EIDITH and imported via
#' @param endpoint The name of the API endpoint: one of "Event",
#' "Animal",  "Specimen", "Test", "Virus", or "TestIDSpecimenID" (for test-specimen cross referencing).  Note these are different
#' than the names of the tables stored locally (which are lowercase and plural).
#' @importFrom dplyr na_if as_data_frame rename_
#' @importFrom stringi stri_trim_both
#' @importFrom purrr map_if
ed_process <- function(dat, endpoint) {
  names(dat) <- fix_names(names(dat))
  dat <- map_if(dat, is.character, ~na_if(stri_trim_both(.), ""))
  dat <- map_if(dat, ~all(. %in% c("yes", "no", NA_character_)),  ~ . == "yes")
  dat <- as_data_frame(dat)
  dat <- do.call(paste0("pp_", endpoint), list(dat))
  return(dat)
}

#' @importFrom dplyr rename_ select_ mutate_ arrange_ recode distinct_
pp_Event <- function(dat) {
  dat <- rename_(dat, .dots = c("event_id"="gains3_event_id",
                                "database_date"="date_last_updated", "date_created" ="crdate", "date_modified"="lmdate"))
  dat <- mutate_(dat, habitat_type = ~recode(habitat_type, `lowland forest`='Lowland forest', `Grassland `='Grassland', `river/stream`='River/stream'))
  dat <- select_(dat, .dots = events_order)
  dat <- arrange_(dat, .dots = "event_id")
  return(distinct_(dat))
}

#' @importFrom dplyr rename_ select_ mutate_  arrange_ left_join starts_with distinct_
pp_Animal <- function(dat) {
  dat <- rename_(dat, .dots = c("event_id"="gains3_event_id", "animal_id"="gains3_sample_unit_id", "animal_id_name"="animal_id_gains",
                                "animal_id_name_fromcountry"="animal_id_from_country",
                                "prioritized_for_testing"="priortized_for_testing",
                                "database_date"="date_last_updated", "date_created" ="crdate", "date_modified"="lmdate"))
  dat <- select_(dat, .dots = c("-sample_unit_id", "-container_id"))
  dat <- left_join(dat, eidith_itis_lookup, by=c("species_scientific_name"="eidith_name")) %>%
    select_(.dots=c("-species_scientific_name", "-class", "-order", "-family", "-genus", "-species")) %>%
    select_(~-starts_with("eidith_")) %>%
    rename_(.dots=c("species_scientific_name"="itis_name",
                    "class"="itis_class",
                    "order"="itis_order",
                    "family"="itis_family",
                    "genus"="itis_genus",
                    "species"="itis_species",
                    "subspecies"="itis_subspecies",
                    "binomial"="itis_binomial"))
  dat <- select_(dat, .dots = animals_order)
  dat <- arrange_(dat, .dots = "animal_id")
  return(distinct_(dat))
}

#' @importFrom dplyr rename_ select_ mutate_ arrange_ distinct_
#' @importFrom stringi stri_replace_first_fixed
pp_Specimen <- function(dat) {
  dat <- rename_(dat, .dots = c("specimen_id_name2"="specimen_id",
                                "specimen_id"="gains3_specimen_id",
                                "animal_id"="gains3_sample_unit_id",
                                "specimen_id_name"="specimen_idunique",
                                "storage_location_address_original" = "storage_location_adress_original",
                                "storage_location_address_current" = "storage_location_adress_current",
                                "database_date"="date_last_updated", "date_created" ="crdate", "date_modified"="lmdate"))
  dat <- mutate_(dat,
                 specimen_id = ~as.integer(specimen_id),
                 specimen_type = ~stri_replace_first_fixed(specimen_type, "Smear, thin", "thin smear") %>%
                                  stri_replace_first_fixed("Smear, thick", "thick smear") %>%
                                  clean_csc(),
                 specimen_type_id = ~stri_replace_first_fixed(specimen_type_id, "Smear, thin", "thin smear") %>%
                                     stri_replace_first_fixed("Smear, thick", "thick smear"),
                 specimen_id_name = ~stri_replace_first_fixed(specimen_id_name, "Lung, Liver", "Lung-Liver") %>%
                                     stri_replace_first_fixed("kushtia", "Kushtia") %>%
                                     despace() %>%
                                     destrange() %>%
                                     reduce_dashes())
  dat <- select_(dat, .dots = specimens_order)
  dat <- arrange_(dat, .dots = "specimen_id")
  return(distinct_(dat))
}

#' @importFrom dplyr rename_ select_ mutate_ arrange_ distinct_
#' @importFrom stringi stri_replace_all_fixed stri_replace_all_regex
pp_Test <- function(dat) {
  dat <- rename_(dat, .dots = c("specimen_id"="gains3_specimen_id", "specimen_id_name"="specimen_name",
                                "database_date"="date_last_updated", "date_created" ="crdate", "date_modified"="lmdate"))
  dat <- mutate_(dat,
                 specimen_id_name = ~stri_replace_all_fixed(specimen_id_name, "Lung, Liver", "Lung-Liver") %>%
                                     stri_replace_all_fixed("110818EKSGbg001", "110818EKSGBG001") %>%
                                     stri_replace_all_fixed("120716EKSGbb001", "120716EKSGBB001") %>%
                                     stri_replace_all_fixed("mx", "MX") %>%
                                     stri_replace_all_regex("^78$", "078a") %>%
                                     destrange() %>%
                                     clean_csc(to_lower=FALSE) %>%
                                     despace() %>%
                                     reduce_dashes(),
                 specimen_type = ~clean_csc(specimen_type))
  dat <- select_(dat, .dots = tests_order)
  dat <- arrange_(dat, .dots = "test_id")
  return(distinct_(dat))
}

#' @importFrom dplyr rename_ select_ mutate_ arrange_ distinct_
pp_Virus <- function(dat) {
  dat <- rename_(dat, .dots = c("virus_id"="gains3_sequence_id",
                                "test_id"="gains3_test_id",
                                "new_genbank_accession" = "genbank_accession_number",
                                "known" = "virus_status",
                                "database_date"="date_last_updated", "date_created" ="crdate", "date_modified"="lmdate"))
  dat <- mutate_(dat,
                 known = ~known=="known",
                 known_genbank_accession = ~get_genbank(interpretation),
                 known_human_risk = ~get_interest(interpretation))
  dat <- select_(dat, .dots = viruses_order)
  dat <- arrange_(dat, .dots = "virus_id")
  return(distinct_(dat))
}

#' @importFrom dplyr rename_ distinct_
pp_TestIDSpecimenID <- function(dat) {
  dat <- rename_(dat, .dots = c("test_id"="gains3_test_id",
                                "specimen_id"="gains3_specimen_id"))
  return(distinct_(dat))
}

#' @importFrom stringi stri_extract_all_regex stri_replace_all_fixed
#' @importFrom dplyr na_if
get_genbank <- function(interpretations) {
  accession <- stri_extract_all_regex(interpretations,"([A-Z]{1,2}_?[0-9]{5,6})")
  accession <- stri_replace_all_fixed(accession, "_", "")
  accession <- na_if(accession, "character(0)")
  return(accession)
}

#' @importFrom stringi stri_detect_regex stri_detect_fixed
get_interest <- function(interpretations) {
  !stri_detect_regex(
    interpretations,
    "[Tt]here is(\\scurrently)? not? evidence(\\sat this time)?(\\sto suggest)?(\\sthat)? ((this virus)|(it))(es)?(\\sa)? (poses?)?(might be)? ((any risk)|(a threat)) to human health.?"
    ) &
  !stri_detect_fixed(interpretations,
              "there is no evidence at this time that Simian adenoviruses pose a threat to human health")
}

events_order <- c(
  "event_id",
  "country",
  "organization",
  "event_date",
  "site_name",
  "primary_interface",
  "event_name",
  "region",
  "state_prov",
  "district",
  "latitude",
  "longitude",
  "habitat_type",
  "habitat_comment",
  "landscape_conversion_gradient",
  "anthropogenic_change",
  "anthropogenic_change_other",
  "anthropogenic_change_secondary",
  "anthropogenic_change_secondary_other",
  "domestic_animals",
  "other_domestic_animals",
  "duration_of_event_days",
  "recorder_name",
  "recorder_other",
  "archived_data",
  "deep_forest_data",
  "source_certainty",
  "other_primary_interface",
  "secondary_interface",
  "other_secondary_interface",
  "reason_for_collection",
  "source_known",
  "event_custom_columns",
  "primary_interface_group",
  "secondary_interface_group",
  "date_modified",
  "date_created",
  "database_date")

animals_order <- c(
  "animal_id",
  "event_id",
  "animal_id_name",
  "species_scientific_name",
  "species_common_name_english",
  "sex",
  "age_class",
  "container_type",
  "quantity",
  "quantity_certainty",
  "quantity_unit",
  "animal_id_name_fromcountry",
  "prioritized_for_testing",
  "sample_date",
  "sample_unit_latitude",
  "sample_unit_longitude",
  "butcher_reduces_blood_contact",
  "circumstance_of_observation",
  "other_condition_of_preservation",
  "origin_known",
  "origin_location",
  "time_since_origin",
  "destination_product",
  "vendor_quantity",
  "price_unit_usd",
  "rate_sale_sold",
  "identified_by",
  "circumstance_observation",
  "animal_classification",
  "species_common_name_local",
  "identification_certainty",
  "tag_band_id",
  "pregnant",
  "with_young",
  "lactating",
  "condition_at_capture",
  "condition_at_release",
  "preservation_method",
  "other_condition_preservation",
  "use",
  "other_use",
  "euthanasia",
  "clinical_signs_gross_findings",
  "disease_suspected",
  "suspected_disease",
  "necropsy_exam",
  "field_necropsy_result",
  "body_measurement_body_part_type",
  "body_measurement_unit",
  "body_measurement_value",
  "part_name",
  "part_description",
  "perceived_rate_of_sale",
  "price_unit_local",
  "vendor_quantity_unit",
  "vendor_quantity_certainty",
  "butchering_activity",
  "butcher_name",
  "butcher_observable_risk_factors",
  "quarter",
  "sample_unit_notes",
  "sample_unit_custom_columns",
  "sample_individual_name",
  "taxagroup",
  "class",
  "order",
  "family",
  "genus",
  "species",
  "subspecies",
  "binomial",
  "date_modified",
  "date_created",
  "database_date")

specimens_order <- c(
  "specimen_id",
  "animal_id",
  "specimen_id_name",
  "specimen_type",
  "sampling_method",
  "aliquot_id",
  "specimen_date",
  "specimen_type_group",
  "specimen_condition",
  "sample_container",
  "storage_medium",
  "labratory_storage_method",
  "storage_location_facility_original",
  "storage_location_in_facility_original",
  "storage_location_address_original",
  "storage_location_facility_current",
  "storage_location_in_facility_current",
  "storage_location_address_current",
  "time_from_collection_to_freezing",
  "specimen_comments",
  "specimen_notes",
  "specimen_custom_columns",
  "specimen_type_id",
  "specimen_id_name2",
  "date_modified",
  "date_created",
  "database_date")

tests_order <- c(
  "test_id",
  "specimen_id_name",
  "pooled",
  "test_type_broad",
  "test_type_specific",
  "test_date",
  "test_result",
  "confirmatory_testing",
  "confirmation_result",
  "diagnostic_laboratory_name",
  "lab_storage_method",
  "lab_submission_date",
  "specimen_type",
  "specimen_type_group",
  "other_test_type",
  "test_requested",
  "other_test_requested",
  "test_requested_protocol",
  "other_test_requested_protocol",
  "methodology_reference",
  "results_date",
  "raw_result",
  "result_descriptor",
  "sequence",
  "virus_name",
  "interpretation",
  "test_status",
  "message_sent_to_country",
  "message_sent_to_govt",
  "govt_approved_release",
  "notes",
  "interpretation_notes",
  "comments",
  "predict_protocol",
  "specimen_id",
  "date_modified",
  "date_created",
  "database_date"
)

viruses_order <- c(
  "virus_id",
  "test_id",
  "viral_family",
  "viral_genus",
  "known",
  "known_human_risk",
  "new_genbank_accession",
  "known_genbank_accession",
  "virus_name",
  "virus_code",
  "sequence",
  "interpretation",
  "date_modified",
  "date_created",
  "database_date"
)
