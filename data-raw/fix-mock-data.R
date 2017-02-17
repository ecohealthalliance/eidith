library(tidyverse)
library(jsonlite)
library(stringi)
devtools::load_all()
P <- rprojroot::find_package_root_file

mock_events <- fromJSON("https://predict2api4devsite.eidith.org/api/modeling/EventMock")
mock_animals <- fromJSON("https://predict2api4devsite.eidith.org/api/modeling/AnimalMock")
mock_specimens <- fromJSON("https://predict2api4devsite.eidith.org/api/modeling/SpecimenMock")
mock_tests <- fromJSON("https://predict2api4devsite.eidith.org/api/modeling/TestMock")
mock_viruses <- fromJSON("https://predict2api4devsite.eidith.org/api/modeling/VirusMock")

real_events <- ed_get_events(postprocess = FALSE)
real_animals <- ed_get_animals(postprocess = FALSE)
real_specimens <- ed_get_specimens(postprocess = FALSE)
real_tests <- ed_get_tests(postprocess = FALSE)
real_viruses <- ed_get_viruses(postprocess = FALSE)
real_testspecimen <- ed_get_testspecimen(postprocess = FALSE)
# names(real_events)
# names(mock_events)
# mock_events lacks CRDate and LMDate values
mock_events_fixed = mock_events %>%
  mutate(CRDate = DateLastUpdated, LMDate = DateLastUpdated)
mock_events_fixed = mock_events_fixed[, names(real_events)]



names(real_animals)
names(mock_animals)[]
mock_animals_fixed = mock_animals %>%
  select(-c(1:3, 5:32), -PrimaryInterfaceGroup, -SecondaryInterfaceGroup) %>%
  rename(GAINS3_SampleUnitID=AnimalID_int, GAINS3_EventID=EventID_int, `Necropsy/exam`=`Necropsy/Exam`) %>%
  mutate(CRDate = DateLastUpdated, LMDate = DateLastUpdated) %>%
  mutate(SampleIndividualName = `AnimalID (GAINS)`, SampleUnitID = `AnimalID (GAINS)`)
mock_animals_fixed = mock_animals_fixed[, names(real_animals)]
# names(real_animals)
# names(mock_animals_fixed)
# names(real_animals)[!names(real_animals) %in% names(mock_animals_fixed)]
# names(mock_animals_fixed)[!names(mock_animals_fixed) %in% names(real_animals)]
#
# names(real_specimens)
extra_spec_fields <- which(names(mock_specimens) %in% names(mock_specimens)[!names(mock_specimens) %in% c(names(real_specimens), "SpecimenID_int", "AnimalID_int")])
mock_specimens_fixed <- mock_specimens %>%
  select(-extra_spec_fields, -GAINS3_SpecimenID) %>%
  rename(GAINS3_SampleUnitID=AnimalID_int, GAINS3_SpecimenID=SpecimenID_int) %>%
  mutate(CRDate = DateLastUpdated, LMDate = DateLastUpdated,
         SpecimenTypeID = SpecimenType, SpecimenNotes = NA)
mock_specimens_fixed = mock_specimens_fixed[, names(real_specimens)]

# names(real_specimens)[!names(real_specimens) %in% names(mock_specimens_fixed)]
# names(mock_specimens_fixed)[!names(mock_specimens_fixed) %in% names(real_specimens)]

# names(real_tests)
# names(mock_tests)
extra_test_fields <-which((!names(mock_tests) %in% c(names(real_tests), "SpecimenIDUnique")))
mock_tests_fixed <- mock_tests %>%
  rename(SpecimenName=SpecimenIDUnique) %>%
  mutate(TestID = testID_int1) %>%
  select(-extra_test_fields) %>%
  mutate(CRDate = DateLastUpdated, LMDate = DateLastUpdated)
mock_tests_fixed = mock_tests_fixed[, names(real_tests)]

# names(real_tests)[!names(real_tests) %in% names(mock_tests_fixed)]
# names(mock_tests_fixed)[!names(mock_tests_fixed) %in% names(real_tests)]
# names(real_viruses)
# names(mock_viruses)

extra_virus_fields <- which((!names(mock_viruses) %in% c(names(real_viruses), "TestID_int", "VirusID_Int")))
mock_viruses_fixed <- mock_viruses %>%
  select(-extra_virus_fields) %>%
  rename(GAINS3_SequenceID=VirusID_Int, GAINS3_TestID=TestID_int) %>%
  mutate(CRDate = DateLastUpdated, LMDate = DateLastUpdated,
         GenbankAccessionNumber = NA, VirusCode=NA, VirusStatus=NA)
mock_viruses_fixed = mock_viruses_fixed[, names(real_viruses)]

mock_viruses_fixed = mock_viruses_fixed %>%
  filter(!is.na(Sequence)) %>%
  distinct(Sequence, .keep_all=TRUE) %>%
  mutate(GAINS3_TestID=382)

# names(real_viruses)[!names(real_viruses) %in% names(mock_viruses_fixed)]
# names(mock_viruses_fixed)[!names(mock_viruses_fixed) %in% names(real_viruses)]



test_interim <- mock_tests %>%
  select(testID_int1, SpecimenID_Int, Pooled, SpecimenID, SpecimenIDUnique) %>%
  separate(SpecimenID, into=c("SpecimenID_1", "SpecimenID_2"), sep = "[\\,]+", fill="right") %>%
  mutate_each(funs(stri_trim_both), SpecimenID_1, SpecimenID_2) %>%
  gather("SpecimentID_no", "SpecimenID", SpecimenID_1, SpecimenID_2) %>%
  mutate(join_id = coalesce(SpecimenIDUnique, SpecimenID)) %>%
  select(testID_int1, join_id)
specimen_interim <- mock_specimens %>%
  select(GAINS3_SpecimenID, SpecimenIDUnique) %>%
  rename(join_id = SpecimenIDUnique)

mock_testspecimen_fixed <- left_join(test_interim, specimen_interim, by="join_id") %>%
  rename(GAINS3_TestID = testID_int1) %>%
  select(-join_id)

mock_testspecimen_fixed = mock_testspecimen_fixed[, names(real_testspecimen)]


raw_mock_data <- list(events = mock_events_fixed,
                      animals = mock_animals_fixed,
                      specimens = mock_specimens_fixed,
                      tests = mock_tests_fixed,
                      viruses = mock_viruses_fixed,
                      testspecimen = mock_testspecimen_fixed)
raw_mock_data <- map(raw_mock_data, as_tibble)

walk2(raw_mock_data, names(raw_mock_data), ~readr::write_csv(.x, P("data-raw", paste0("mock_", .y, ".csv"))))

processed_mock_data <- purrr::map2(raw_mock_data,
                                   c("Event", "Animal",  "Specimen", "Test", "Virus", "TestIDSpecimenID" ),
                                   ed_process)


