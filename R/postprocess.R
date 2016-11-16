#' Standard post-processing of EIDITH exports
#'
#' This function takes raw EIDITH dat tables and does minor common
#' post-processing tasks.
#'
#' @param dat The dat as exported from EIDITH and imported via
#' @param endpoint The name of the endpoint or dat table: one of "Event",
#' "Animal",  "Specimen", "Test", or "Virus".
#' @export
ed_postprocess <- function(dat, endpoint) {
  #names(dat) <- camel_to_snake(names(dat))
  dat <- do.call(paste0("pp_", endpoint), list(dat))
  return(dat)
}

camel_to_snake <- function(CamelNames) {
  Snake__names <- gsub("([a-z])([A-Z])", "\\1_\\L\\2", CamelNames, perl = TRUE)
  snake__names <- tolower(Snake__names)
  snake_names <- gsub(" ", "_", snake__names, fixed=TRUE)
  snake_names <- gsub("__", "_", snake_names, fixed=TRUE)
  return(snake_names)
}


#' @importFrom dplyr rename_ select_ mutate_ recode
pp_Event <- function(dat) {
  # dat <- dplyr::rename_(dat, .dots = c("event_id"="gains3_event_id", "organization_id"="organization_idowner"))
  # dat <- dplyr::select_(dat, .dots = c("event_id", "region", "country", "site_name", "event_date",
  #                                        "state_prov", "district", "latitude", "longitude", "habitat_type",
  #                                        "habitat_comment", "landscape_conversion_gradient", "anthropogenic_change",
  #                                        "anthropogenic_change_other", "anthropogenic_change_secondary",
  #                                        "anthropogenic_change_secondary_other", "domestic_animals", "other_domestic_animals",
  #                                        "duration_of_event_days", "recorder_name", "recorder_other",
  #                                        "organization", "organization_id", "archived_dat", "deep_forest_dat", "source_certainty",
  #                                        "primary_interface", "other_primary_interface", "secondary_interface",
  #                                        "other_secondary_interface", "reason_for_collection", "source_known",
  #                                        "event_custom_columns", "primary_interface_group", "secondary_interface_group",
  #                                        "date_last_updated", "event_name"))
  dat <- dplyr::mutate_(dat, habitat_type = ~recode(habitat_type, `lowland forest`='Lowland forest', `Grassland `='Grassland', `river/stream`='River/stream'))
  dat <- dplyr::mutate_(dat, archived_dat = ~(archived_dat == "yes"), deep_forest_dat = ~(deep_forest_dat == "yes"))
  return(dat)
}

#' @importFrom dplyr rename_ select_ mutate_
pp_Animal <- function(dat) {
  # dat <- dplyr::rename_(dat, .dots = c("event_id"="gains3_event_id", "sample_id"="sample_unit_id", "animal_id"="`animal_id_(gains)`"))
  # dat <- dplyr::select_(dat, .dots = c("sample_id", "event_id", "class", "order",
  #                                        "family", "genus", "species", "animal_classification", "taxagroup", "species_scientific_name",
  #                                        "species_common_name_english", "species_common_name_local", "identification_certainty",
  #                                        "container_type", "quantity", "quantity_certainty", "quantity_unit",
  #                                        "sample_unit_id", "animal_id", "animal_id_from_country", "container_id",
  #                                        "priortized_for_testing", "sample_date", "sample_unit_latitude",
  #                                        "sample_unit_longitude", "butcher_reduces_blood_contact", "circumstance_of_observation",
  #                                        "other_condition_of_preservation", "origin_known", "origin_location",
  #                                        "time_since_origin", "destination_product", "vendor_quantity",
  #                                        "price_unit_usd", "rate_sale_sold", "identified_by", "circumstance_observation",
  #                                        "tag_band_id", "sex", "age_class", "pregnant", "with_young",
  #                                        "lactating", "condition_at_capture", "condition_at_release",
  #                                        "preservation_method", "other_condition_preservation", "use",
  #                                        "other_use", "euthanasia", "clinical_signs/gross_findings", "disease_suspected",
  #                                        "suspected_disease", "necropsy/exam", "field_necropsy_result",
  #                                        "body_measurement_body_part_type", "body_measurement_unit", "body_measurement_value",
  #                                        "part_name", "part_description", "perceived_rate_of_sale", "price_unit_local",
  #                                        "vendor_quantity_unit", "vendor_quantity_certainty", "butchering_activity",
  #                                        "butcher_name", "butcher_observable_risk_factors", "quarter",
  #                                        "sample_unit_notes", "sample_unit_custom_columns",  "date_last_updated", "sample_individual_name"))
  dat2 <- left_join(dat, eidith_itis_lookup, by=c("SpeciesScientificName"="eidith_name")) %>%
    select_(.dots=c("-SpeciesScientificName", "-class", "-order", "-family", "-genus", "-species")) %>%
    select_(~-starts_with("eidith_")) %>%
    rename_(.dots=c("SpeciesScientificName"="itis_name",
           "class"="itis_class",
           "order"="itis_order",
           "family"="itis_family",
           "genus"="itis_genus",
           "species"="itis_species",
           "subspecies"="itis_subspecies",
           "binomial"="itis_binomial"))


  return(dat2)
}

pp_Specimen <- function(dat) {
  return(dat)
}

pp_Test <- function(dat) {
  return(dat)
}

pp_Virus <- function(dat) {
  return(dat)
}
