eidith_base_url <- "https://predict2api.eidith.org/api/modeling/"

eidith2_base_url <- "https://predict2api.eidith.org/api/Extract/"

event_non_ehp_cols <- c("Country", "SiteName", "ConcurrentSamplingSite", "District", "StateProv",
                        "SiteLatitude", "SiteLongitude", "EventLatitude", "EventLongitude", "EventDate",
                        "Season", "DurationDays", "EventName", "Recorder", "Organization", "HumanDensityImpact",
                        "DiseaseTransmissionInterfaces", "OutbreakName", "VeterinarianCare", "SamplingAreaSize",
                        "HumansPresent", "RodentsPresent", "BatsPresent", "NHPPresent", "BirdsPresent", "CarnivoresPresent",
                        "UngulatesPresent", "PangolinsPresent", "PoultryPresent", "GoatsPresent", "CamelsPresent",
                        "SwinePresent", "CattlePresent", "DogsPresent", "CatsPresent", "drinkingWaterShared",
                        "bathingWaterShared", "ToiletsAvailable", "DrinkingWaterSource", "AverageTripToWater",
                        "InsectVectors", "VectorControlMeasures", "CommunityEngagement", "CommunityEngagementDate",
                        "CommunityEngagementNotes", "NbrBeds", "IsDeepForest", "Notes")

human_non_ehp_cols <- c("Country", "SiteName", "ConcurrentSamplingSite", "District", "StateProv",
                        "SiteLatitude", "SiteLongitude", "EventLatitude", "EventLongitude",
                        "EventDate", "Season", "DurationDays", "EventName", "DiseaseTransmissionInterfaces",
                        "GAINS4_SampleUnitID", "Q1 ParticipantID", "Q3 DateOfInterview", "Q4 BeginTimeInterview",
                        "Q5 EndTimeInterview", "Q6 InterviewCity", "Q6 InterviewStateProv", "Q6 InterviewLatitude",
                        "Q6 InterviewLongitude", "Q7 Gender", "Q8 Age", "Q9 LiveCity", "Q9 LiveStateProv",
                        "Q9 LiveLatitude", "Q9 LiveLongitude", "Q10 LengthLived", "Q11 PeopleInDwelling",
                        "Q12 ChildrenInDwelling", "Q13 MalesInDwelling", "Q14 RoomsInDwelling", "Q15 DwellingPermanentStructure?",
                        "Q16 DrinkingWaterSource", "Q17 WaterTreated?", "Q18 WaterTreatment", "Q19 WaterUsedByAnimals?",
                        "Q20 DedicatedLocationForWaste?", "Q21 FoodStorageContainers", "DemographicNotes", "Q22 HighestEducation",
                        "Q23 HighestEducatinMother", "Q24 Livelihood(s)", "LivelihoodGroup(s)IfOther", "Q25 MostTimeSpent",
                        "ModulesCompleted", "Q26 JobPosition", "Q27 WorkLocationCity", "Q27 WorkLocationProv", "Q27 WorkLocationLatitude",
                        "Q27 WorkLocationLongitude", "LivelihoodNotes", "Q28 Treatment", "Q29 SymptomsLife", "Q30 HadSymptomsInLastYear",
                        "Q31 SymptomsInLastYear", "Q32 SicknessCause", "Q33 SymptomsInLastYearOtherPeople", "Q34 SymptomsInLastYearOtherPeople",
                        "Q35 IllnessDeath", "MedicalHistoryNotes", "Q36 Travelled", "Q37 More than 6", "Q37 TravelledCity Loc 1",
                        "Q37 Latitude Loc 1", "Q37 Longitude Loc 1", "Q37 TravelledCity Loc 2", "Q37 Latitude Loc 2", "Q37 Longitude Loc 2",
                        "Q37 TravelledCity Loc 3", "Q37 Latitude Loc 3", "Q37 Longitude Loc 3", "Q37 TravelledCity Loc 4", "Q37 Latitude Loc 4",
                        "Q37 Longitude Loc 4", "Q37 TravelledCity Loc 5", "Q37 Latitude Loc 5", "Q37 Longitude Loc 5", "Q37 TravelledCity Loc 6",
                        "Q37 Latitude Loc 6", "Q37 Longitude Loc 6", "Q38 Travel Reason", "MovementNotes", "Q39 PetInDwellingLife", "Q39 PetInDwellingLastYear",
                        "Q40 HandleAnimalsLife", "Q40 HandleAnimalsLastYear", "Q41 RaisedAnimalsLife", "Q41 RaisedAnimalsLastYear", "Q42 SharedWaterLife",
                        "Q42 SharedWaterLastYear", "Q43 AnimalFecesFoodLife", "Q43 AnimalFecesFoodLastYear", "Q44 AnimalsInFoodLife", "Q44 AnimalsInFoodLastYear",
                        "Q45 AnimalsInDwellingLife", "Q45 AnimalsInDwellingLastYear", "Q46 CookedMeatLife", "Q46 CookedMeatLastYear", "Q47 EatenRawMeatLife",
                        "Q47 EatenRawMeatLastYear", "Q48 EatenSickAnimalLife", "Q48 EatenSickAnimalLastYear", "Q49 EatenDeadAnimalLife", "Q49 EatenDeadAnimalLastYear",
                        "Q50 SoldDeadAnimalLife", "Q50 SoldDeadAnimalLastYear", "Q51 ScratchedBittenLife", "Q51 ScratchedBittenLastYear", "Q52 HuntedAnimalLife",
                        "Q52 HuntedAnimalLastYear", "Q53 SlaughtedAnimalLife", "Q53 SlaughtedAnimalLastYear", "Q54 ScratchedBittenAction", "Q55 RiskOpenWound",
                        "Q56 Contact with rodents/shrews", "Q56 Contact with bats", "Q56 Contact with non-human primates", "Q56 Contact with birds",
                        "Q56 Contact with carnivores", "Q56 Contact with ungulates", "Q56 Contact with pangolins", "Q56 Contact with poultry/other fowl",
                        "Q56 Contact with goats/sheep", "Q56 Contact with camels", "Q56 Contact with swine", "Q56 Contact with cattle/buffalo", "Q56 Contact with horses",
                        "Q56 Contact with dogs", "Q56 Contact with cats", "Q57 WorriedAboutDisease", "SpecimensCollected", "Organization",
                        "SiteLatitudeUnrounded", "SiteLongitudeUnrounded", "EventLatitudeUnrounded", "EventLongitudeUnrounded", "InterfacesAnimalsSampled", "InterfacesHumansSampled", "ParticipantID", "Livelihood(s)", "MostTimeSpent", "Q1 AnimalsHunted", "Q2 MethodsUsedToHuntTrap", "Q3 RodentsHuntTrapPurpose", "Q3 BatsHuntTrapPurpose", "Q3 NHPHuntTrapPurpose", "Q3 BirdsHuntTrapPurpose", "Q3 CarnivoresHuntTrapPurpose", "Q3 UngulatesHuntTrapPurpose", "Q3 PangolinsHuntTrapPurpose", "Q4 ExposedToBloodLastYear", "Q5 ScratchedBittenLastYear", "Q6 OutbreakLastYear", "Q7 OutbreakDeaths", "Q8 ActionWhenDeadAnimalFound", "Q9 TransportDeadAnimal", "Q10 PPEUsed", "Q11 WhichPPE", "Q12 WhenPPEUsed", "Notes",
                        "Q1 LiveOnSite", "Q2 #WorkAtSite", "Q3 HowLongWorked", "Q4 AnimalsRaised", "Q5 LiveAnimalsStoredNight", "Q6 QuarantineForNewAnimals", "Q7 OnSiteFoodProduction", "Q8 WhoPaysGrowCrops", "Q9 BushMeatOnsite", "Q10 MeatSource", "Q11 ConsumeBushMeatOnSite", "Q12 PPEUsed", "Q13 WhichPPE", "Q14 WhenPPEUsed", "Q15 UseDisinfectantToClean", "Q16 DisinfectantsUsedFor", "Q17 AnimalEnclosuresCleanedFrequency", "Q18 ViscaraRefuse", "Q19 DesignatedAreaForAnimalWaste", "Q20 DesignatedAreaForAnimalWasteUsed", "Q21 AnimalsReceiveVetCareinYear", "Q22 HealthOfficalInspectedAnimals", "Q23 ActionWhenAnimalsSick", "Q24 AnimalsQuarantinedOrDestroyed", "Q25 WhichAnimalsQuarantinedOrDestroyed", "Q26 OutbreakInYear", "Q27 RodentsOutbreakDeaths", "Q27 BatsOutbreakDeaths", "Q27 NHPOutbreakDeaths", "Q27 BirdsOutbreakDeaths", "Q27 CarnivoresOutbreakDeaths", "Q27 UngulatesOutbreakDeaths", "Q27 PangolinsOutbreakDeaths", "Q27 PoultryOutbreakDeaths", "Q27 GoatsOutbreakDeaths", "Q27 CamelsOutbreakDeaths", "Q27 SwineOutbreakDeaths", "Q27 CattleOutbreakDeaths", "Q27 HorsesOutbreakDeaths", "Q27 DogsOutbreakDeaths", "Q27 CatsOutbreakDeaths", "Q28 AnimalsRaidFood", "Q29 WhichAnimalsRaidFood", "Q30 RaidingPrevention", "notes")

#' Return he names of the tables in the EIDITH database.
#'
#' @rdname endpoints
#' @aliases p1_api_endpoints p2_api_endpoints
#' @export
p1_api_endpoints <- function() {
  c("Event", "Animal", "Specimen", "Test", "Virus",
    "TestIDSpecimenID")
}

#' This function returns the names of the tables in the PREDICT-2 EIDITH database.
#' @rdname endpoints
#' @aliases p1_api_endpoints p2_api_endpoints
#' @export
p2_api_endpoints <- function() {
  c("Event", "Animal", "Specimen", "AnimalProduction", "CropProduction", "Dwellings",
    "ExtractiveIndustry", "MarketValueChain", "NaturalAreas", "WildlifeRestaurant", "ZooSanctuary",
    "Human", "HumanCropProduction", "HumanAnimalProduction","HumanExtractiveIndustry", "HumanHospitalWorker",
    "HumanHunter", "HumanMarket", "HumanRestaurant", "HumanSickPerson", "HumanTemporarySettlements", "HumanZoo",
    "Test", "TestDataInterpreted", "TestDataSerology", "Behavioral", "Training"#,
    #"HumanEHP", "HumanAnimalProductionEHP", "HumanHunterEHP"

  )
}

#' This function returns the names of PREDICT-2 countries in the EIDITH database.
#' @rdname countries
#' @aliases predict_countries eha_countries ehp_countries
#' @export
predict_countries <- function() {
  c("Bangladesh", "DR Congo", "Republic of Congo", "Ivory Coast", "Cameroon", "China", "Egypt", "Ethiopia",
    "Ghana", "Guinea", "Indonesia", "India", "Jordan", "Kenya", "Cambodia", "Lao PDR", "Liberia", "Myanmar",
    "Mongolia", "Malaysia, Peninsular", "Malaysia, Sabah", "Nepal", "Rwanda", "Sierra Leone", "Senegal",
    "South Sudan", "Thailand", "Tanzania", "Uganda", "Vietnam"
  )
}

#' This function returns the names of EHA PREDICT-2 countries in the EIDITH database.
#' @rdname countries
#' @aliases predict_countries eha_countries ehp_countries
#' @export
eha_countries <- function() {
  c("Bangladesh", "Republic of Congo", "Ivory Coast", "China", "Egypt", "Indonesia", "India", "Jordan",
    "Liberia", "Malaysia, Peninsular", "Malaysia, Sabah", "South Sudan", "Thailand"
  )
}

#' This function returns the names of Ebola Host Project PREDICT-2 countries in the EIDITH database.
#' @rdname countries
#' @aliases predict_countries eha_countries ehp_countries
#' @export
ehp_countries <- function() {
  c(
    "Liberia", "Sierra Leone", "Guinea"
  )
}

create_empty_p2_table <- function(e2){
  meta <- ed2_metadata()
  headers <- filter(meta, endpoint2 == e2, replacement_name %in% "DROP" == FALSE) %>%
    mutate(new_name = ifelse(is.na(replacement_name), auto_processed_name, replacement_name)) %>%
    pull(new_name)
  df <- data.frame(matrix(ncol = length(headers), nrow = 0))
  names(df) <- headers
  return(df)
}

create_empty_p1_table <- function(e1){
  meta <- ed_metadata()
  headers <- filter(meta, endpoint == e1, replacement_name %in% "DROP" == FALSE) %>%
    mutate(new_name = ifelse(is.na(replacement_name), auto_processed_name, replacement_name)) %>%
    pull(new_name)
  df <- data.frame(matrix(ncol = length(headers), nrow = 0))
  names(df) <- headers
  return(df)
}


#' Functions to download EIDITH tables via API
#'
#' These functions download data directly from the EIDITH API.  They require
#' [authorization][ed_auth].  They can be useful for comparing local data
#' against data updated in the database, or processed vs. unprocessed data.
#' The [table functions][ed_table] load data from the local database instead,
#' and are thus faster and work without an internet connection.
#'
#' @param verbose Show a progress bar and other messages?
#' @param postprocess Should data be cleaned via [ed_process()] or returned raw?
#' @param header_only Return only the table header.  Useful for checking if
#' API access works
#' @param lmdate_from filter records by earliest date, in YYYY-MM-DD format
#' @param lmdate_to filter records by latest date, in YYYY-MM-DD format (see details)
#' @param ... additional arguments passed to [httr::GET()]
#' @return a [tibble][tibble::tibble()]-style data frame
#' @rdname ed_get
#' @name ed_get
#' @importFrom httr GET status_code progress authenticate content modify_url
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble
#' @export
ed_get <- function(endpoint, verbose=interactive(), postprocess=TRUE,
                   header_only=FALSE, lmdate_from="2000-01-01",
                   lmdate_to=Sys.Date() + 1, auth=NULL, ...) {

  url <- modify_url(url =  paste0(eidith_base_url, endpoint),
                    query = list(header_only = ifelse(header_only, "y", "n"),
                                 lmdate_from = lmdate_from,
                                 lmdate_to = lmdate_to))
  if(verbose) {
    pbar <- progress()
    cat_line(red(paste("Downloading PREDICT-1", endpoint, "table...\n")))
  } else {
    pbar <- NULL
  }

  if(is.null(auth)) auth <- ed_auth()

  request <- GET(url=url, authenticate(auth[1], auth[2], type="basic"),
                 pbar, ...)

  if(status_code(request) == 401) {
    stop("Unauthorized (HTTP 401). Your username or password do not match an account.
See ?ed_auth.")
  }

  if(status_code(request) == 403) {
    stop("Forbidden (HTTP 403). Your account does not have access permissions.
Contact technology@eidith.org about permissions. See ?ed_contact.")
  }

  if(verbose) cat_line("Importing...\n")
  data <- fromJSON(content(request, as = "text", encoding="UTF-8"))

  if(header_only) {
    return(data)
  } else {
    data <- as_tibble(data)
  }

  if(nrow(data) == 0){
    data <- create_empty_p1_table(endpoint)
    return(data)
  }

  if("ExceptionMessage" %in% names(data)){
    cat_line(red(paste0("Download for the ", endpoint, " table failed. See ?ed_contact for support. \n")))
    return(invisible(0))
  }

  if(postprocess){
    data <- tryCatch(ed_process(data, endpoint),
                     error = function(e){
                       cat_line(red(paste0("Error: The fields in the ", endpoint, " download are not as expected.  See message for details and ?ed_contact for support.\n")))
                       return(invisible(NULL))
                     })

  }

  return(data)
}


#' @param verbose Show a progress bar and other messages?
#' @param header_only Return only the table header.  Useful for checking if
#' API access works
#' @param lmdate_from filter records by earliest date, in YYYY-MM-DD format
#' @param lmdate_to filter records by latest date, in YYYY-MM-DD format (see details)
#' @param ... additional arguments passed to [httr::GET()]
#' @return a [tibble][tibble::tibble()]-style data frame
#' @rdname ed2_get
#' @name ed2_get


#' @noRd
#' @importFrom httr GET status_code progress authenticate content modify_url
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble
#' @importFrom purrr map imap map_df map_lgl
#' @export
ed2_get <- function(endpoint2, country=NULL, postprocess=TRUE, verbose=interactive(),
                    header_only=FALSE, auth=NULL, ...) {

  url_end <- ifelse(endpoint2 %in% c("TestDataInterpreted", "TestDataSerology"), "", "Data")

  if(is.null(country)){
    url <-  paste0(eidith2_base_url, "Extract", endpoint2, url_end) # url when country is null (gets all countries)
  } else {
    if(any(!country %in% predict_countries())) { # if country is specified, make sure it is recognized
      stop(paste(
        "Not recognized PREDICT country: ",
        paste(country[!country %in% predict_countries()], collapse = ", ")
      ))
    }
    url <- map(country, function(x){ # make list of urls for specified countries
      modify_url(url =  paste0(eidith2_base_url, "Extract", endpoint2, url_end),
                 query = list(country = paste0("'", x, "'")))
    })
  }

  if(endpoint2 %in% c("HumanEHP", "HumanAnimalProductionEHP", "HumanHunterEHP")){
    if(is.null(country)){
      endpoint_mod <- gsub("EHP", "", endpoint2)
      # url list for all three EHP countries
      url <- list(modify_url(url =  paste0(eidith2_base_url, "Extract", endpoint_mod, "Data"),
                             query = list(country = "'Liberia'")),
                  modify_url(url =  paste0(eidith2_base_url, "Extract", endpoint_mod, "Data"),
                             query = list(country = "'Sierra Leone'")),
                  modify_url(url =  paste0(eidith2_base_url, "Extract", endpoint_mod, "Data"),
                             query = list(country = "'Guinea'")))
    }else{
      if(any(!country %in% ehp_countries())) stop("EHP tables available only for Liberia, Sierra Leone, and Guinea")
      url <- map(url, ~gsub("EHP", "", .x)) # remove EHP from url.  this applies for cases when country is specified.
    }
  }

  if(verbose) {
    pbar <- progress()
    cat_line(red(paste("Downloading PREDICT-2", endpoint2, "table...\n")))
  } else {
    pbar <- NULL
  }

  if(is.null(auth)) auth <- ed_auth()

  request <- imap(url, function(x, y) {
    if(y != length(url)){pbar = NULL}
    GET(url = x, authenticate(auth[1], auth[2], type="basic"), pbar)
  })

  if(any(map_lgl(request, ~status_code(.x) == 401))) {
    stop("Unauthorized (HTTP 401). Your username or password do not match an account.
         See ?ed_auth.")
  }

  if(any(map_lgl(request, ~status_code(.x) == 403))) {
    stop("Forbidden (HTTP 403). Your account does not have access permissions.
         Contact technology@eidith.org.")
  }

  if(verbose) cat_line("Importing...")
  data <- map_df(request, ~fromJSON(content(.x, as = "text", encoding="UTF-8")))

  if(endpoint2 == "Event"){ # do not include ebola names in event tbl (regardless of whether EHP country)
    data <- data[,colnames(data) %in% event_non_ehp_cols]
  }

  if(endpoint2 %in% c("Human", "HumanAnimalProduction", "HumanHunter")){
    if(any(!country  %in% ehp_countries()) | is.null(country)){ # do not include ebola names in human tbls unless only ehp data is downloaded
      data <- data[,colnames(data) %in% human_non_ehp_cols]
    } else {
      # leave data fields as is but change endpoint2 for metadata
      endpoint2 <- paste0(endpoint2, "EHP")
    }
  }

  if(header_only) {
    return(data)
  } else {
    data <- as_tibble(data)
  }

  if(nrow(data) == 0){
    data <- create_empty_p2_table(endpoint2)
    return(data)
  }

  if("ExceptionMessage" %in% names(data)){
    cat_line(red(paste0("Download for the ", endpoint2, " table failed. See ?ed_contact for support.")))
    return(invisible(0))
  }

  if(postprocess){
    data <- tryCatch(ed2_process(data, endpoint2),
                     error = function(e){
                       cat_line(red(paste0("Error: The fields in the ", endpoint2, " download are not as expected. See message for details and ?ed_contact for support.")))
                       cat_line(e)
                       return(invisible(0))
                     },  warning = function(w){
                       cat_line(red(paste0("Warning: The fields in the ", endpoint2, " download are not as expected. See message for details and ?ed_contact for support.")))
                       cat_line(w)
                       return(invisible(0))
                     })

  }

  #if(postprocess) data <- ed2_process(data, endpoint2)

  return(data)
}


#' @rdname ed_get
#' @export
ed_get_events <- function(verbose=interactive(), postprocess=TRUE,
                          header_only=FALSE, lmdate_from="2000-01-01",
                          lmdate_to=Sys.Date() + 1, ...) {
  ed_get("Event", verbose, postprocess, header_only, lmdate_from, lmdate_to, ...)
}


#' @rdname ed_get
#' @export
ed_get_animals <- function(verbose=interactive(), postprocess=TRUE,
                           header_only=FALSE, lmdate_from="2000-01-01",
                           lmdate_to=Sys.Date() + 1, ...) {
  ed_get("Animal", verbose, postprocess, header_only, lmdate_from, lmdate_to, ...)
}

#' @rdname ed_get
#' @export
ed_get_specimens <- function(verbose=interactive(), postprocess=TRUE,
                             header_only=FALSE, lmdate_from="2000-01-01",
                             lmdate_to=Sys.Date() + 1, ...) {
  ed_get("Specimen", verbose, postprocess, header_only,
         lmdate_from, lmdate_to, ...)
}

#' @rdname ed_get
#' @export
ed_get_tests <- function(verbose=interactive(), postprocess=TRUE,
                         header_only=FALSE, lmdate_from="2000-01-01",
                         lmdate_to=Sys.Date() + 1, ...) {
  ed_get("Test", verbose, postprocess, header_only, lmdate_from, lmdate_to, ...)
}

#' @rdname ed_get
#' @export
ed_get_viruses <- function(verbose=interactive(), postprocess=TRUE,
                           header_only=FALSE, lmdate_from="2000-01-01",
                           lmdate_to=Sys.Date() + 1, ...) {
  ed_get("Virus", verbose, postprocess, header_only,
         lmdate_from, lmdate_to, ...)
}

#' @rdname ed_get
#' @export
ed_get_testspecimen <- function(verbose=interactive(), postprocess=TRUE,
                                header_only=FALSE, lmdate_from="2000-01-01",
                                lmdate_to=Sys.Date() + 1, ...) {
  ed_get("TestIDSpecimenID", verbose, postprocess, header_only,
         lmdate_from, lmdate_to, ...)
}
