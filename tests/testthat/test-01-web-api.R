

# for(endpoint in eidith:::endpoints) {
#   dat = eidith::ed_get(endpoint, verbose=FALSE, postprocess = FALSE)
#   feather::write_feather(dat, paste0(tolower(endpoint), ".feather"))
# }
# rm(dat)
library(httr)
context("eidith")

test_that("Server API works", {

  GET(paste0(eidith:::eidith_base_url, eidith:::endpoints[2]), authenticate(eidith_user(), eidith_pwd()), )
  expect_true(TRUE)

})

# Check that server is available
# Check authentication works - correct logons work but wrong ones don't
#    - Future - check that security scopes are correct
# Check that names of tables are as expected
# Check that
#
s =animal_ %>%
  group_by(`AnimalID (GAINS)`) %>%
  mutate(n= n()) %>%
  group_by() %>%
  filter(n > 1) %>%
  arrange(`AnimalID (GAINS)`) %>%
  left_join(events_)


specimens %>% group_by(SpecimenIDUnique) %>%
  mutate(n = n()) %>%
  group_by() %>%
  filter(n > 1) %>%
  #select(1:5, SpecimenIDUnique) %>%
  arrange(SpecimenIDUnique, GAINS3_SampleUnitID) %>%
  print(n=500)

events %>%
#  filter(DeepForestData=="yes") %>%
  use_series(EventDate) %>%
  as.Date %>%
  table(useNA="always") %>%
  sort %>%
  tail

events %>% filter(EventDate=="2014-01-06")


vir %>%
  as.Date %>%
  table(useNA="always") %>%
  sort %>%
  tail

specimens %>% group_by(GAINS3_SpecimenID) %>%
  mutate(n = n()) %>%
  group_by() %>%
  filter(n > 1) %>%
  #select(1:5, SpecimenIDUnique) %>%
  arrange(SpecimenIDUnique, GAINS3_SampleUnitID) %>%
  print(n=500)
viruses
