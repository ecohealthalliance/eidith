eidith2_base_url <- "https://predict2api.eidith.org/api/Extract/"

# event ehp names to exclude from event table
event_ehp_names <- c("HumanPopulation", "NumEbolaCases", "NumEbolaSurvivors", "DateFirstCase", "DateLastCase", "EbolaTreatmentCenter", "QuarantinedDuringOutbreak", "QuarantineDate", "WeeksQuarantine", "PercentageQuarantined", "FoodMarketClosed", "WeeksFoodMarketClosed", "HealthClinicClosed", "WeeksHealthClinicClosed", "EbolaBurialGround", "BurialGroundLocation", "BatsinBuildings", "BatsinTrees", "BatsinCaves", "BatsSoldInMarket", "BatsinBuildingsnPopulation")


#' This function returns the names of the tables in the PREDICT-2 EIDITH database.
#' @rdname endpoints
#' @aliases p1_api_endpoints p2_api_endpoints
#' @export
p2_api_endpoints <- function() {
  c("Event", "Animal", "Specimen", "AnimalProduction", "CropProduction", "Dwellings",
    "ExtractiveIndustry", "MarketValueChain", "NaturalAreas", "WildlifeRestaurant", "ZooSanctuary",
    "Human", "HumanEHP","HumanCropProduction", "HumanAnimalProduction", "HumanAnimalProductionEHP", "HumanExtractiveIndustry", "HumanHospitalWorker",
    "HumanHunter", "HumanHunterEHP", "HumanMarket", "HumanRestaurant", "HumanSickPerson", "HumanTemporarySettlements", "HumanZoo",
    "Test", "TestDataInterpreted", "TestDataSerology", "Behavioral", "Training"
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
    "South Sudan", "Thailand", "Tanzania", "Uganda", "Vietnam", "Peru", "Bolivia", "Brazil", "Mexico", "Gabon", "Colombia"
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
    "Liberia",  "Guinea" #, "Sierra Leone"
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

#' @param endpoint2 Table name
#' @param country Which countries to get? Default is NULL, which will return all countries
#' @param p1_data Include P1 results?
#' @param verbose Show a progress bar and other messages?
#' @param header_only Return only the table header.  Useful for checking if API access works
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
ed2_get <- function(endpoint2, country=NULL, p1_data=FALSE, postprocess=TRUE,
                    verbose=interactive(), header_only=FALSE, auth=NULL, ...) {

  stop("API access capabilities are unavailable. Use import_local_db() instead.")

  # should endpoint2 have "data" appeneded in the url call?
  url_end <- ifelse(endpoint2 %in% c("TestDataInterpreted", "TestDataSerology"), "", "Data")

  # ehp handling when country is given - if only specifying ehp countries, get ehp-specific data, otherwise remove ehp country from results
  if(endpoint2 %in% c("Human", "HumanAnimalProduction", "HumanHunter") & !is.null(country)){
    if(all(country %in% ehp_countries())) {
      endpoint2 <- paste0(endpoint2, "EHP")
    } else {
      country <- country[!country %in% ehp_countries()]
    }
  }

  # ehp modules to be called from the human table, so create endpoint_mod for url call.  also specify countries for ehp call if they are not already specified.
  if(endpoint2 %in% c("HumanEHP", "HumanAnimalProductionEHP", "HumanHunterEHP")){
    endpoint_mod <- "Human"
    if(is.null(country)){
      country <- ehp_countries()
    } else {
      # filter out non ehp countries
      country <- country[country %in% ehp_countries()]
    }
  }else{
    endpoint_mod <- endpoint2
  }

  # url when country is null (gets all countries)
  url <-  paste0(eidith2_base_url, "Extract", endpoint_mod, url_end)

  # url when country is specified
  if(!is.null(country)){

    # check: if country is specified, make sure it is recognized
    if(any(!country %in% predict_countries())) {
      stop(paste(
        "Not recognized PREDICT country: ",
        paste(country[!country %in% predict_countries()], collapse = ", ")
      ))
    }

    # list of urls for specified countries
    url <- map(country, function(x){
      modify_url(url =  paste0(eidith2_base_url, "Extract", endpoint_mod, url_end),
                 query = list(country = x))
    })
  }

  # add modules parameters for ehp modeules
  if(endpoint2 == "HumanAnimalProductionEHP") {url <- map(url, ~modify_url(url = .x,
                                                                           query = list(modules = "AP")))}
  if(endpoint2 == "HumanHunterEHP") {url <- map(url, ~modify_url(url = .x,
                                                                 query = list(modules = "HT")))}

  # combine with p1 table if specified
  if(p1_data & endpoint2 %in% c("Event", "Animal", "Specimen", "Test", "TestDataInterpreted")){
    url <-  map(url, ~modify_url(url = .x,
                                 query = list(p1data = 1)))
  }

  # test table - include sequence column
  if(endpoint2 == "Test"){
    url <-  map(url, ~modify_url(url = .x,
                                 query = list(sequence=1 )))
  }

  # get data
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
    data <- data[,!colnames(data) %in% event_ehp_names]
  }

  if(endpoint2 ==  "HumanAnimalProductionEHP"){
    data <- data %>%
      filter(!is.na(`(Animal Production Module) Q1 LiveOnSite`))
  }

  if(endpoint2 ==  "HumanHunterEHP"){
    data <- data %>%
      filter(!is.na(`(Hunter Module) Q1 AnimalsHunted`))
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
                     })

  }

  return(data)
}
