#This script generates a lookup table to generate matches between EIDITH
#species and standardized ITIS species names

library(magrittr)
library(tidyverse)
library(stringi)
library(taxize)
library(urltools)
library(pbapply)
library(ritis)
library(googlesheets)
P <- rprojroot::find_package_root_file
devtools::load_all()


#Convenience functions
stri_trans_tosentence <- function(string) {
  #Capitalize first letter of string
  stri_paste(stri_trans_toupper(stri_sub(string, 1L, 1L)), stri_trans_tolower(stri_sub(string, 2L, -1L)))
}

replace_blank_with_na <- function(string) {
  string[nchar(string) == 0] <- as.character(NA)
  return(string)
}

# A wrapper around itis_search to sanitize inputs and deal with failed queries
itis_search_safe <- function(term, fuzz=0.4, binomial_only = FALSE) {

  sanitized_term <- term %>% stri_trim_both() %>%
    stri_replace_all_regex("^(.*)(\\s(s+p|cf)\\.)+", "$1")# %>%  #drop sp., ssp., cf.
  # stri_replace_all_regex("(\\(|\\))", "")
  if(binomial_only) {
    sanitized_term <- stri_replace_first_regex(sanitized_term, "(\\w+\\s+\\w+)\\s+\\w+", "$1")  #get rid of subspecies names
  }
  term2 <- sanitized_term %>%
    stri_replace_all_regex("( |\\(|\\))", "\\\\$1") #escape problem characters

  query = url_encode(paste0("nameWOInd:", term2, "~", as.character(fuzz))) #build search query
  result = safely(itis_search)(q=query)
  if(is.null(result$result) || nrow(result$result) == 0) {
    return(data_frame(original_term=term, sanitized_term=sanitized_term))
  } else {
    return(mutate(result$result, original_term=term, sanitized_term=sanitized_term))
  }
  return(term)
}

#load data
animals <- ed_animals(postprocess = FALSE)

#get fallback table
manual_matches <- gs_read(gs_url("https://docs.google.com/spreadsheets/d/1n_Q-Rhi3HQfxj-a9ay3ZLae6hMaTXAQNS6DFl8NFCO4")) %>%
  filter(!is.na(itis_name))

#select unique taxa
taxa <- animals %>% distinct_("SpeciesScientificName", .keep_all=TRUE) %>%
  select(SpeciesScientificName, class, order, family, genus, species) %>%
  mutate_all(stri_trim_both)

#replace known problem species with fallbacks
taxa <- taxa %>%
  left_join(select(manual_matches, SpeciesScientificName, itis_name), by="SpeciesScientificName") %>%
  mutate(use_name = if_else(is.na(itis_name), SpeciesScientificName, itis_name))



#Send each binomial to the ITIS Solr iterface, get back data frames of responses
#(This takes a few minutes)
taxa <- taxa %>%
  mutate(itis_results = pblapply(taxa$use_name, function(x) suppressMessages(itis_search_safe(x))))

taxa2 <- taxa %>% #If the subspecies has no match, try just the species
  mutate(itis_results = map_if(itis_results, ~(nrow(.)==1 & ncol(.)==2), function(x) suppressMessages(itis_search_safe(x$original_term, binomial_only = TRUE))))

taxa3 <- taxa2 %>% #Unpack the ITIS string into the whole taxonomy for each record
  mutate(itis_results = map_if(itis_results, ~!(nrow(.)==1 & ncol(.)==2), function(x) {
    x %>%
      mutate(hier = map(hierarchySoFarWRanks, ~data_frame(rankname = stri_extract_all_regex(., "(?<=\\$)\\w+(?=\\:)")[[1]],
                                                          rankvalue = stri_extract_all_regex(., "(?<=\\w\\:)[^\\:\\$]+(?=\\$)")[[1]]))) %>%
      unnest(hier) %>%
      spread(rankname, rankvalue)
  }))

taxa4 <- taxa3 %>% #Matches outside our core kingdom/classes invalid
  mutate(itis_results = map_if(itis_results,  ~!(nrow(.)==1 & ncol(.)==2), function(x) {
    filter(x, Kingdom=="Animalia", Class %in% unique(taxa3$class))
  }))

taxa5 <- taxa4 %>% #If there is an exact match, take that
  mutate(itis_results = map_if(itis_results,  ~!(nrow(.)==1 & ncol(.)==2), function(x) {
    y = filter(x, nameWOInd == sanitized_term)
    if(nrow(y)==1) return(y) else return(x)
  }))


taxa6 <- taxa5 %>% #If there is only one valid result, use that one
  mutate(itis_results = map_if(itis_results,  ~(!is.null(.[["usage"]]) && sum(.$usage=="valid")==1), function(x) {
    y = filter(x, usage=="valid")
  }))

taxa7 <- taxa6 %>% #If there are multiple exact matches, pick the valid one
  mutate(itis_results = map_if(itis_results,  ~(nrow(.) > 1 && sum(.$nameWOInd == .$sanitized_term) > 0), function(x) {
    y = filter(x, usage=="valid" & nameWOInd == sanitized_term)
  }))

taxa8 <- taxa7 %>%
  mutate(match = !map_lgl(itis_results, ~nrow(.) < 1 | nrow(.) > 1 | ncol(.)==2))

misses <- taxa8 %>% filter(!match)

taxa9 <- taxa8 %>%
  unnest(itis_results) %>%
  select(-itis_name) %>%
  rename(eidith_name=SpeciesScientificName,
         eidith_class=class,
         eidith_order=order,
         eidith_family=family,
         eidith_genus=genus,
         eidith_species=species,
         itis_name=nameWInd,
         itis_class=Class,
         itis_order=Order,
         itis_family=Family,
         itis_genus=Genus,
         itis_species=Species,
         itis_subspecies=Subspecies) %>%
  select(eidith_name, eidith_class, eidith_order, eidith_family, eidith_genus, eidith_species,
         itis_name, itis_class, itis_order, itis_family, itis_genus, itis_species, itis_subspecies) %>%
  mutate(itis_species=stri_extract_first_regex(itis_species, "(?<=\\s)\\w+$"),
         itis_subspecies=stri_extract_first_regex(itis_subspecies, "(?<=\\s)\\w+$"),
         itis_name = stri_trim_both(stri_paste(itis_genus, itis_species, if_else(is.na(itis_subspecies), "", itis_subspecies), sep=" ")),
         itis_binomial = stri_paste(itis_genus, itis_species, sep=" "))

  taxa9$itis_name =  case_when(!is.na(taxa9$itis_species) ~ taxa9$itis_name,
                               !is.na(taxa9$itis_genus) ~ paste(taxa9$itis_genus, "sp."),
                               !is.na(taxa9$itis_family) ~ paste(taxa9$itis_family, "sp."),
                               !is.na(taxa9$itis_order) ~ paste(taxa9$itis_order, "sp."),
                               !is.na(taxa9$itis_class) ~ paste(taxa9$itis_class, "sp."),
                               TRUE ~ taxa9$eidith_name)

eidith_itis_lookup <- taxa9

  # taxa9 %>% filter(eidith_name != itis_name   |
  #                eidith_class != itis_class |
  #                eidith_order != itis_order |
  #                eidith_family != itis_family|
  #                eidith_genus != itis_genus |
  #                eidith_species != itis_species ) %>% View
write_csv(eidith_itis_lookup, P("data-raw/eidith_itis_lookup.csv"))
devtools::use_data(eidith_itis_lookup, internal = TRUE, overwrite = TRUE)
