library(eidith)
library(tidyverse)
library(stringr)
library(parallel)

events <- ed_events(postprocess = F)

animals <- ed_animals(postprocess = F)

specimens <- ed_specimens(postprocess = F)

tests <- ed_tests(postprocess = F)

viruses <- ed_viruses(postprocess = F)

local_data <- list(events, animals, specimens, tests, viruses)

saveRDS(local_data, "local_data.rds")

ae2 <- full_join(animals, events, by = c("GAINS3_EventID"))

which(animals$GAINS3_EventID %in% events$GAINS3_EventID == FALSE)
# no animals without events

aes2 <- full_join(specimens, ae, by = c("GAINS3_SampleUnitID"))

which(specimens$GAINS3_SampleUnitID %in% animals$GAINS3_SampleUnitID == FALSE)
#8 specimens without an animal assigned

unlinked_specimens <- as_tibble(rbind(specimens[which(specimens$GAINS3_SampleUnitID %in% animals$GAINS3_SampleUnitID == FALSE), ]))

tv <- full_join(tests, viruses, by = c("TestID" = "GAINS3_TestID"))

which(viruses$GAINS3_TestID %in% tests$TestID == FALSE)
#two viruses without tests

unlinked_viruses <- as_tibble(rbind(viruses[which(viruses$GAINS3_TestID %in% tests$TestID == FALSE),]))



y <- strsplit(x$SpecimenName, " , ")
unlist(y)
length(unlist(y))

Ctests <- tests

Ctests$Specimens <- str_split(Ctests$SpecimenName, ",")

Ctests$Specimens <- lapply(Ctests$Specimens, str_trim)

all_specs <- unlist(Ctests$Specimens)

length(all_specs) - length(unique(all_specs))
# there are 573614 repeats

length(unique(all_specs))
#61080 unique specimens tested

u_all_specs <- unique(all_specs)

length(which(u_all_specs %in% specimens$SpecimenIDUnique == FALSE))
# 546 tests contain specimens that are not in specimens df

length(which(u_all_specs %in% specimens$SpecimenIDUnique))
# 60534 tests contain specimens that are in specimens df


###################################################################################


presence_check <- mclapply(Ctests$Specimens, function(x) x %in% specimens$SpecimenIDUnique, mc.cores = 4)

presence_check2 <- mclapply(Ctests$Specimens, function(x) x %in% specimens$SpecimenIDUnique, mc.cores = 4)



absent2 <- sapply(presence_check2, function(x) which(sum(x == FALSE) > 0))

missing_rows2 <- which(absent2 > 0)

unlinked_tests <- Ctests[missing_rows2,]
table(unlinked_tests$DiagnosticLaboratoryName)


"LAP13-Q0001-Lung" %in% specimens$SpecimenIDUnique

View(specimens[which(str_detect(specimens$SpecimenIDUnique, "LAP13-Q0001")==TRUE),])

View(specimens[which(str_detect(specimens$SpecimenIDUnique, "Lung, Liver")==TRUE),])


specimens[163554,]

#############################################

sum(str_detect(specimens$SpecimenIDUnique, "MAN-3107"))

"MAN-3097" %in% specimens$SpecimenIDUnique

"MAN-3102" %in% specimens$SpecimenIDUnique

"MAN-3105" %in% specimens$SpecimenIDUnique

"MAN-3107" %in% specimens$SpecimenIDUnique
#These 4 are in tests but not in specimens

View(specimens[which(str_detect(specimens$SpecimenIDUnique, "MAN-310")==TRUE),])

all_specs[1]

tests$SpecimenName[994]

despaced <-

decommaspace <- function(c){
  str_replace_all(c, "(\\s+)?,(\\s+)?", ",")
}

despace <- function(c){
  str_replace_all(c, "\\s", "-")
}

View(specimens[which(str_detect(specimens$SpecimenIDUnique, "\\s+")),])


despaced <- despace(despaced)

View(despaced[which(str_detect(despaced, "\\s+"))])



View(tests[which(str_detect(tests$SpecimenName, ",?\\s+\\d+")),])

"WCS06811J100086HEd1" %in% specimens$SpecimenIDUnique

View(specimens[which(str_detect(specimens$SpecimenIDUnique, "WCS06811J100086")),])




unlinked_tests <- pp_Test(unlinked_tests)

df <- data.frame(old = unlinked_tests$SpecimenName, new = unlinked_tests$CorrectedSpecimenNames)


#############################
#Testing new processing

cale_tests <- pp_Test(tests)

cale_specimens <- pp_Specimen(specimens)

cale_tests$Specimens <- str_split(cale_tests$CorrectedSpecimenNames, ",")

cale_all_specs <- unlist(cale_tests$Specimens)

cale_all_specs <- unique(cale_all_specs)

missing_cale <- cale_all_specs[which(cale_all_specs %in% cale_specimens$CorrectedSpecimenIDUnique == FALSE)]

cale_all_specs[which(cale_all_specs %in% cale_specimens$CorrectedSpecimenIDUnique == FALSE)]

"Kushtia" %in% cale_specimens$SpecimenIDUnique

View(cale_specimens[which(str_detect(cale_specimens$CorrectedSpecimenIDUnique,"Kushtia")),])
View(specimens[which(str_detect(specimens$SpecimenIDUnique, "Kushtia")),])

View(cale_specimens[which(str_detect(cale_specimens$CorrectedSpecimenIDUnique, "Kushtia")),])



View(cale_tests)

presence_check3 <- mclapply(cale_tests$Specimens, function(x) x %in% cale_specimens$CorrectedSpecimenIDUnique, mc.cores = 4)

absent <- sapply(presence_check3, function(x) which(sum(x == FALSE) > 0))

missing_rows <- which(absent > 0)

unlinked_cale <- cale_tests[missing_rows,]

"WCS06811J100086HEd1-2" %in% cale_specimens$CorrectedSpecimenIDUnique

View(cale_specimens[which(str_detect(cale_specimens$CorrectedSpecimenIDUnique, "KH11.P.0018.V")),])


check_processing(specimens, tests)

View(tests[which(str_detect(tests$SpecimenName, "KH11.P.0018.V")),])


localdata <- readRDS("localdata.rds")

specimens <- ed_specimens(postprocess = F)


