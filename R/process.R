#' Standard post-processing of EIDITH exports
#'
#' This function takes raw data downloaded from EIDITH and puts it through
#' various preprocessing and cleaning steps.  In general there is no need to
#' call this function directly - it is called by both [ed_db_download()] and the [direct download functions][ed_get()].
#'
#' Steps taken to clean the data include:
#'
#' -   Converting variable names from `camelCase` to `snake_case` to make it easy
#' to distinguish between raw and cleaned data.
#' -   Converting some variable names to clearer ones:  all `_id` variables are
#' numeric primary keys, other identifiers now go by `_id_name`.
#' -   Where there are multiple `_id_name`-type columns that are very similar except for a small set of cases, we drop all but one for ease of use. These can be retrieved from raw data if needed.
#' -   Dropping columns that are entirely blank
#' -   Dropping redundant columns
#' -   Cleaning up whitespace and capitalization variability
#' -   Re-arranging table order to put the most pertinent information first.
#' -   Normalizing all animal taxonomic information to match the [ITIS](https://www.itis.gov/) database.
#' -   Coercing some free-form entries (e.g. `specimen_type`) to a standard set of categories
#' -   Converting yes/no fields to TRUE/FALSE
#' -   Fixing spelling errors
#' -   Extracting common TRUE/FALSE variables from free-form text of viral interpretation (Genbank numbers, whether virus is known, whether virus is known to affect humans).
#'
#' @param dat The data as exported from EIDITH and imported via the [ed_get()] functions (without preprocessing).
#' @param endpt The name of the API URL endpoints: one of "Event",
#' "Animal",  "Specimen", "Test", "Virus", or "TestIDSpecimenID" (for test-specimen cross referencing).  Note these are different
#' than the names of the tables stored locally (which are lowercase and plural).
#' @importFrom dplyr na_if as_data_frame rename_ %>% mutate_ select_ if_else data_frame arrange_ full_join
#' @importFrom magrittr use_series
#' @importFrom stringi stri_trim_both
#' @importFrom purrr map_if
ed_process <- function(dat, endpt) {
  emd <-filter_(ed_metadata(), ~ endpoint == endpt) %>%
    mutate_(new_name = ~if_else(is.na(replacement_name), auto_processed_name, replacement_name))
  expected_fields <- emd %>% filter_(~!is.na(original_name)) %>% use_series("original_name")
  # First, check that the data is as expected
  unexpected_fields <- names(dat)[!(names(dat) %in% expected_fields)]
  missing_fields <- expected_fields[!(expected_fields %in% names(dat))]
  wrn_con <- "\nRe-install the eidith package and try again. If warning persists see ?ed_contact"

  if(length(missing_fields)) warning("Expected fields missing in ", endpt, " download: ", paste0(missing_fields, collapse=", "), ".", wrn_con)
  if(length(unexpected_fields)) warning("Unexpected fields  in ", endpt, " download: ", paste0(unexpected_fields, collapse=", "), ". These fields will be dropped.", wrn_con)

  # Drop any other fields we want to drop
  drop_cols <- filter_(emd, ~replacement_name == "DROP")[["original_name"]]
  dat <- select_(dat, .dots = as.list(which(!names(dat) %in% c(drop_cols, unexpected_fields))))

  # Change the field names to the local, simplified versions
  used <- data_frame(original_name=names(dat)) %>%
    full_join(filter_(emd, ~replacement_name != "DROP" | is.na(replacement_name)), by="original_name")
  names(dat) <- used %>% filter_(~!is.na(original_name)) %>% use_series("new_name")

  # General cleanups
  dat <- map_if(dat, is.character, ~na_if(stri_trim_both(.), ""))
  dat <- map_if(dat, ~all(. %in% c("yes", "no", NA_character_)),  ~ . == "yes")
  dat <- as_data_frame(dat)

  # Table-specific cleanups
  process_fn <- get(paste0("pp_", endpt), envir=asNamespace("eidith"))
  dat <- process_fn(dat)

  # Sort
  names_order <-  arrange_(used, "order")[["new_name"]]
  dat <- select_(dat, .dots = names_order)
  dat <- arrange_(dat, names_order[1])
  return(distinct_(dat))
}

#' @importFrom dplyr rename_ select_ mutate_ arrange_ recode distinct_
pp_Event <- function(dat) {
  if("habitat_type" %in% names(dat)) {
    dat <- mutate_(dat, habitat_type = ~recode(habitat_type, `lowland forest`='Lowland forest', `Grassland `='Grassland', `river/stream`='River/stream'))
  }
  return(dat)
}

#' @importFrom dplyr rename_ select_ mutate_  arrange_ left_join starts_with distinct_
pp_Animal <- function(dat) {
  if(all(c("species_scientific_name", "class", "order", "family", "genus", "species") %in% names(dat))) {
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
  }
  return(dat)
}

#' @importFrom dplyr rename_ select_ mutate_ arrange_ distinct_
#' @importFrom stringi stri_replace_first_fixed
pp_Specimen <- function(dat) {

  if("specimen_id" %in% names(dat)) {
    dat <- mutate_(dat, specimen_id = ~as.integer(specimen_id))
  }

  if("specimen_type" %in% names(dat)) {
    dat <- mutate_(dat, specimen_type = ~stri_replace_first_fixed(specimen_type, "Smear, thin", "thin smear") %>%
                     stri_replace_first_fixed("Smear, thick", "thick smear") %>%
                     clean_csc())
  }

  if("specimen_type_id" %in% names(dat)) {
    dat <- mutate_(dat, specimen_type_id = ~stri_replace_first_fixed(specimen_type_id, "Smear, thin", "thin smear") %>%
                     stri_replace_first_fixed("Smear, thick", "thick smear"))
  }

  if("specimen_id_name" %in% names(dat)) {
    dat <- mutate_(dat, specimen_id_name = ~stri_replace_first_fixed(specimen_id_name, "Lung, Liver", "Lung-Liver") %>%
                     stri_replace_first_fixed("kushtia", "Kushtia") %>%
                     despace() %>%
                     destrange() %>%
                     reduce_dashes())
  }
  return(dat)
}

#' @importFrom dplyr rename_ select_ mutate_ arrange_ distinct_ left_join
#' @importFrom stringi stri_replace_all_fixed stri_replace_all_regex
pp_Test <- function(dat) {

  if("diagnostic_laboratory_name" %in% names(dat)) {
    dat <- left_join(dat, ed_lab_shortnames, by="diagnostic_laboratory_name")
  }
  if("specimen_id_names" %in% names(dat)) {
    dat <- mutate_(dat,
                   specimen_id_names = ~stri_replace_all_fixed(specimen_id_names, "Lung, Liver", "Lung-Liver") %>%
                     stri_replace_all_fixed("110818EKSGbg001", "110818EKSGBG001") %>%
                     stri_replace_all_fixed("120716EKSGbb001", "120716EKSGBB001") %>%
                     stri_replace_all_fixed("mx", "MX") %>%
                     stri_replace_all_regex("^78$", "078a") %>%
                     destrange() %>%
                     clean_csc(to_lower=FALSE) %>%
                     despace() %>%
                     reduce_dashes())
  }

  if("specimen_type" %in% names(dat)) {
    dat <- mutate_(dat, specimen_type = ~clean_csc(specimen_type))
  }
  return(dat)
}

#' @importFrom dplyr rename_ select_ mutate_ arrange_ distinct_
pp_Virus <- function(dat) {

  if("known" %in% names(dat)) {
    dat <- mutate_(dat, known = ~known=="known")
  }

  if("interpretation" %in% names(dat)) {
    dat <- mutate_(dat, known_genbank_accession = ~get_genbank(interpretation),
                   evidence_human_infection = ~get_interest(interpretation))
  }
  return(dat)
}

#' @importFrom dplyr rename_ distinct_
pp_TestIDSpecimenID <- function(dat) {
  return(dat)
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
