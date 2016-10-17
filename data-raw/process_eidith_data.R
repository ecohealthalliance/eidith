library(tidyverse) #install via devtools::install_github('hadley/tidyverse')
library(stringi)
library(purrr)

#----import_tables-----------------------
p1_files = readLines("data-raw/eidith_eha_predict1_file-submissions_Sep09_1413.csv", encoding="windows-1252")
p1_files = p1_files[p1_files!=""]
p1_files = stri_replace_last_regex(p1_files, "<a href=\"([^\"]+)\">Latest Version</a>", "https://eidith.org$1")
contlines = which(stri_detect_regex(p1_files, "^\","))
p1_files[contlines - 1] <- paste0(p1_files[contlines - 1], p1_files[contlines])
p1_files = p1_files[-contlines]
p1_files = stri_replace_all_regex(p1_files, "(?<!(^|,))\"(?!($|,))", "'")

tmp = tempfile()
writeLines(p1_files, tmp)
p1_files = read_csv(tmp)


p1_species = read_csv("data-raw/eidith_eha_predict1_species-names_Sep09_1414.csv", locale = locale(encoding="windows-1252"))

p1_events =
  readLines("data-raw/eidith_eha_predict1_events_Sep09_1415.csv", encoding="windows-1252") %>%
  stri_replace_all_regex("(?<!(^|,))\"(?!($|,))", "'")
tmp = tempfile()
writeLines(p1_events, tmp)
p1_events = read_csv(tmp)

p1_animals = read_csv("data-raw/eidith_eha_predict1_animals-sampled_Sep09_1416.csv",
                      col_types = cols(SampleNameForCountry = col_character()),
                      locale = locale(encoding="windows-1252"))


p1_specimens = read_csv("data-raw/eidith_eha_predict1_specimens_Sep09_1420.csv",
                        locale = locale(encoding="windows-1252"))


p1_tests = readLines("data-raw/eidith_eha_predict1_tests_Sep09_1425.csv", encoding="windows-1252") %>%
  stri_replace_all_regex("(?<!(^|,))\"(?!($|,))", "'")
tmp=tempfile()
writeLines(p1_tests, tmp)
p1_tests = read_csv(tmp)

p1_results = readLines("data-raw/eidith_eha_predict1_interpreted-results_Sep09_1431.csv", encoding="windows-1252") %>%
  stri_replace_all_fixed("acute \"common-cold\"", "accute 'common-cold'")
tmp=tempfile()
writeLines(p1_results, tmp)
p1_results = read_csv(tmp)


p1_sequences = read_csv("data-raw/eidith_eha_predict1_sequences_Sep09_1433.csv",
                        locale = locale(encoding="windows-1252"))

p1_columbia = read_csv("data-raw/eidith_eha_predict1_columbia-submissions_Sep09_1434.csv",
                       locale = locale(encoding="windows-1252"))


tables = list(
  p1_files=p1_files,
  p1_speciess=p1_species,
  p1_animals=p1_animals,
  p1_events=p1_events,
  p21_specimens=p1_specimens,
  p1_tests=p1_tests,
  p1_results=p1_results,
  p1_sequences=p1_sequences,
  p1_columbia=p1_columbia
)

table_names = map(tables, names)
all_names = table_names %>% unlist %>% unique %>% sort

fields = c(list(field=all_names), map(table_names, ~all_names %in% .)) %>%
  as.data.frame()
fields = fields %>% mutate(in_tables = rowSums(fields[,-1])) %>%
  tbl_df

pfields = fields %>% filter(in_tables > 1) %>% arrange(desc(in_tables)) %>% print(n=100)
# EIDITH EXPORT NOTES
# Quotes in Notes and DownloadFile field are unescaped

camel_to_snake = . %>%
    stri_replace_all_regex("(?<!(^|[A-Z]))([A-Z])(?![A-Z])", "_$2") %>%
    stri_replace_all_fixed("__", "_") %>%
    stri_trans_tolower()

camel_to_snake_names = function(obj) {
  names(obj) <- camel_to_snake(names(obj))
  return(obj)
}

#----organize_tables-----------------------
all_dupes = function(data, field) {
  dd = which(duplicated)
}
p1_files2 = p1_files %>%
  camel_to_snake_names() %>%
  rename(im_contact_person = imcontact_person)

p1_species2 = p1_species %>%
  camel_to_snake_names() %>%
  mutate(english_name = stri_replace_all_fixed(english_name, "’", "'")) %>%
  mutate(english_name = stri_replace_all_fixed(english_name, "Rusty", "rufous")) %>%
  mutate(english_name = stri_trans_totitle(english_name, type="sentence")) %>%
  mutate(genus = if_else(is.na(genus) & stri_detect_fixed(scientific_name, " "), stri_extract_first_regex(scientific_name, "^[^\\s]+(?=\\s)"), genus)) %>%
  mutate(species = if_else(is.na(species) & stri_detect_fixed(scientific_name, " "), stri_extract_first_regex(scientific_name, "(?<= )[^ ]+$"), species)) %>%
  mutate(genus = if_else(is.na(class) & is.na(genus), scientific_name, genus)) %>%
  mutate(pcountry = paste0(" (", country, ")")) %>%
  mutate(species_id = paste0(scientific_name,
                             if_else(is.na(species) | (species == "sp."), pcountry, ""))) %>%
  select(-pcountry) %>%
  mutate(species = if_else(species == "sp.", as.character(NA), species)) %>%
  arrange(species_id) %>%
  distinct(scientific_name, english_name, class, order, family, genus, species, taxa_group, species_id, .keep_all=TRUE)

map(p1_species2, ~length(unique(.))) %>% unlist
dd = sort(unique(c(which(duplicated(p1_species2$species_id)),
                   which(duplicated(p1_species2$species_id, fromLast=TRUE)))))

p1_species2[dd,] %>% print(n=70)


p1_events2 = p1_events %>%
  select(-GAINS3_OrganizationName) %>%
  rename(event_id = EventName) %>%
  camel_to_snake_names()

p1_animals2 = p1_animals %>%
  select_(.dots=names(p1_animals)[!map_lgl(p1_animals, function(x) all(is.na(x)))]) %>% #remove empty columns
  mutate(pcountry = paste0(" (", Country, ")")) %>%
  mutate(species_id = paste0(ScientificName,
                             if_else(is.na(Species) | (Species == "sp."), pcountry, ""))) %>%
  select(-pcountry) %>%
  select(-SiteName, -StateProv, -District, -Country, -PrimaryInterface, -HabitatType, -LandscapeConversionGradient, - DomesticAnimals, -Organization, -ArchiveData, -AnthropogenicChange) %>% #remove event data, check if long/lat are different than event
  # Remove species information
  rename(event_id = EventName, animal_id = SampleUnitName) %>%
  camel_to_snake_names()   #Check if IsPublic matches between animals and events,


map(p1_animals2, function(x) {
  if(length(unique(x)) < 20) return(table(x, useNA="always")) else return(sum(is.na(x))/length(x))
})

map(p1_animals2, ~length(unique(.))) %>% unlist  #Two exta sentries by  sample_unit_name, which should be the animal_id.  Looks like they're entry dupes.  Eliminate the pregnant male and the one without the note of the missing tail.

dd = which(duplicated(p1_animals2$sample_unit_name))
p1_animals2[sort(c(dd, dd-1)),]
p1_specimens2 = p1_specimens %>%
  select_(.dots=names(p1_specimens)[!map_lgl(p1_specimens, function(x) all(is.na(x)))]) %>%
  select(-SiteName, -StateProv, -District, -Country, -PrimaryInterface, -HabitatType, -LandscapeConversionGradient, - DomesticAnimals, -Organization, -ArchiveData, -AnthropogenicChange) %>% #remove event data, check if long/lat are different than event
  select(-EventName, -TaxaGroup, -Species, -SpeciesCommonNameEnglish, -SampleNameForCountry) %>% #remove animal data
  rename(specimen_id = SpecimenID, animal_id = SampleUnitName) %>%
  camel_to_snake_names()

map(p1_specimens2, ~length(unique(.))) %>% unlist
map(p1_specimens2, function(x) {
  if(length(unique(x)) < 20) return(table(x, useNA="always")) else return(sum(is.na(x))/length(x))
})

p1_tests2a = p1_tests %>%
  rename(Animal_ID_GAINS = `AnimalID (GAINS)`)
p1_tests2 = p1_tests2a %>%
  select_(.dots=names(p1_tests2a)[!map_lgl(p1_tests2a, function(x) all(is.na(x)))]) %>%
  select(-SiteName, -StateProv, -Region, -District, -Country, -PrimaryInterface, -Organization) %>%
  select(-EventName, -TaxaGroup, -SpeciesScientificName) %>% #remove animal data
  camel_to_snake_names() #remove empty columns


map(p1_tests2, ~length(unique(.))) %>% unlist
map(p1_tests2, function(x) {
  if(length(unique(x)) < 20) return(table(x, useNA="always")) else return(sum(is.na(x))/length(x))
})


p1_results2 = p1_results %>%
  select(-SiteName, -EventName, -EventDate, -StateProv, -District, -Country, -Region, -Organization, -PrimaryInterface, -TaxaGroup, -SpeciesScientificName, -SpecimenID, -Pooled, -DiagnosticLaboratoryName, -LabStorageMethod, -LabSubmissionDate, -TestDate, -TestTypeBroad, -TestTypeSpecific, -TestRequested, -OtherTestRequested, -TestRequestedProtocol, -OtherTestRequestedProtocol, -MethodologyReference, -ResultsDate, -TestResult, -RawResult, -ConfirmatoryTesting, -ConfirmationResult, -Interpretation, -Sequence, -VirusName, -TestStatus)


map(p1_results2, ~length(unique(.))) %>% unlist
map(p1_results2, function(x) {
  if(length(unique(x)) < 20) return(table(x, useNA="always")) else return(sum(is.na(x))/length(x))
})

p1_sequences %>% select(-Sequence)
map(p1_sequences, ~length(unique(.))) %>% unlist
map(p1_sequences, function(x) {
  if(length(unique(x)) < 20) return(table(x, useNA="always")) else return(sum(is.na(x))/length(x))
})
