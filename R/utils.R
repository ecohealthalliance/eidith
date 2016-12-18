#' @importFrom magrittr %>%
#' @importFrom stringi stri_replace_all_regex stri_replace_all_fixed stri_trans_tolower
fix_names <- function(names) {
  new_names <- names %>%
    stri_replace_all_regex("([a-z])([A-Z])", "$1_$2") %>%
    stri_replace_all_fixed("'", "") %>%
    stri_replace_all_fixed("\"", "") %>%
    stri_replace_all_fixed("%", "percent") %>%
    make.names() %>%
    stri_replace_all_regex("[.]+", "_") %>%
    stri_replace_all_regex("[_]+", "_") %>%
    stri_trans_tolower() %>%
    stri_replace_all_regex("_$", "")
  dupe_count <- purrr::map_int(seq_along(new_names), function(i) {
    sum(new_names[i] == new_names[1:i])
  })
  new_names[dupe_count > 1] <- paste(new_names[dupe_count > 1],
                                     dupe_count[dupe_count > 1], sep = "_")
  return(new_names)
}

#' @importFrom stringi stri_trans_tolower stri_split_regex stri_paste stri_sort
#' @importFrom purrr map_chr
clean_csc <- function(csc, to_lower=TRUE) {
  if(to_lower) csc <- stri_trans_tolower(csc)
  csc <- stri_split_regex(csc, "\\s*[\\,\\|]\\s*")
  csc <- map_chr(csc,
                 ~stri_paste(stri_sort(., na_last=TRUE),
                             sep = ",", collapse=","))
}

#' @importFrom stringi stri_replace_all_regex
decommaspace <- function(c){
  stri_replace_all_regex(c, "(\\s+)?,(\\s+)?", ",")
}

#' @importFrom stringi stri_replace_all_regex
despace <- function(c){
  stri_replace_all_regex(c, "\\s+", "-")
}

#' @importFrom stringi stri_replace_all_regex
destrange <- function(c){
  stri_replace_all_regex(c, "1,2$","1-2")
}

#' @importFrom stringi stri_replace_all_regex
reduce_dashes <- function(c){
  stri_replace_all_regex(c, "-+","-")
}


#' @importFrom lubridate fast_strptime
quicktime <- function(datestring) {
  as.Date(fast_strptime(datestring, format='%Y-%m-%d'))
}

#' @importFrom lubridate fast_strptime
quicktime2 <- function(datestring) {
  as.POSIXct(fast_strptime(datestring, format='%Y-%m-%dT%H:%M:%OS'))
}
