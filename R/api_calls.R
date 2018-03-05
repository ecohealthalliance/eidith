eidith_base_url <- "https://predict2api.eidith.org/api/modeling/"

eidith2_base_url <- "https://predict2api.eidith.org/api/Extract/"

endpoints2 <- c("Event", "Animal", "Specimen", "AnimalProduction", "CropProduction", "Dwellings",
                "ExtractiveIndustry", "MarketValueChain", "NaturalAreas", "WildlifeRestaurant", "ZooSanctuary",
                "Human", "HumanCropProduction", "HumanAnimalProduction", "HumanExtractiveIndustry", "HumanHospitalWorker",
                "HumanHunter", "HumanMarket", "HumanRestaurant", "HumanSickPerson", "HumanTemporarySettlements", "HumanZoo", "Test", "TestDataInterpreted", "TestDataSerology")

endpoints <- c("Event", "Animal", "Specimen", "Test", "Virus",
               "TestIDSpecimenID")


create_empty_p2_table <- function(e2){
  meta <- ed2_metadata()
  headers <- filter(meta, endpoint2 == e2, replacement_name %in% "DROP" == FALSE) %>%
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
Contact technology@eidith.org.")
  }

  if(verbose) cat_line("Importing...\n")
  data <- fromJSON(content(request, as = "text", encoding="UTF-8"))

  if(header_only) {
    return(data)
  } else {
    data <- as_tibble(data)
  }

  # if(nrow(data) == 0){
  #   message((paste0("Download for the ", endpoint, " table failed. Please contact technology@eidith.org")))
  #   return(invisible(0))
  # }

  if("ExceptionMessage" %in% names(data)){
    cat_line(red(paste0("Download for the ", endpoint, " table failed. Please contact technology@eidith.org\n")))
    return(invisible(0))
  }

  if(postprocess){
    data <- tryCatch(ed_process(data, endpoint),
                     error = function(e){
                       cat_line(red(paste0("Error: There are unexpected fields in the ", endpoint, " download. Please contact technology@eidith.org.\n")))
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
#' @export
ed2_get <- function(endpoint2, postprocess=TRUE, verbose=interactive(),
                   header_only=FALSE, lmdate_from="2000-01-01",
                   lmdate_to=Sys.Date() + 1, auth=NULL, ...) {

  if(endpoint2 == "TestDataInterpreted" | endpoint2 == "TestDataSerology"){
    url <- modify_url(url = paste0(eidith2_base_url, "Extract", endpoint2),
                      query = list(header_only = ifelse(header_only, "y", "n"),
                                   lmdate_from = lmdate_from,
                                   lmdate_to = lmdate_to))
  }else{
  url <- modify_url(url = paste0(eidith2_base_url, "Extract", endpoint2, "Data"),
                    query = list(header_only = ifelse(header_only, "y", "n"),
                                 lmdate_from = lmdate_from,
                                 lmdate_to = lmdate_to))
  }

  if(verbose) {
    pbar <- progress()
    cat_line(red(paste("Downloading PREDICT-2", endpoint2, "table...\n")))
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
         Contact technology@eidith.org.")
  }

  if(verbose) cat_line("Importing...")
  data <- fromJSON(content(request, as = "text", encoding="UTF-8"))

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
    cat_line(red(paste0("Download for the ", endpoint2, " table failed. Please contact technology@eidith.org")))
    return(invisible(0))
  }

  if(postprocess){
   data <- tryCatch(ed2_process(data, endpoint2),
      error = function(e){
  cat_line(red(paste0("Error: There are unexpected fields in the ", endpoint2, " download. Please contact technology@eidith.org.")))
  cat_line(e)
        return(invisible(0))
        },  warning = function(w){
  cat_line(red(paste0("Warning: There are unexpected fields in the ", endpoint2, " download. Please contact technology@eidith.org.")))
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
