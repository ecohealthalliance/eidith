#' Expand multiple-response fields into long data form
#'
#' @param df The data frame containing the field to expand
#' @param col The column name to split into long form
#' @param other_details Whether other category should be consolidated and a new column with other details added. Defaults to TRUE.
#'
#' @importFrom tidyr %>% unnest
#' @importFrom dplyr mutate
#' @importFrom stringr str_split str_trim str_detect str_match str_replace_all
#' @importFrom rlang enquo quo_name !!
#'
#' @return A long form dataframe with `col` field separated into repeated entries.
#' @rdname ed2_expand_long
#' @export
#' @examples
#' \dontrun{
#' humans <- ed2_human()
#' humans_reduced <- dplyr::select(humans, event_name, participant_id, travel_reason)
#' humans_reduced_long <- ed2_expand_long(humans_reduced, travel_reason)
#' }
ed2_expand_long <- function(df, col, other_details = TRUE){
  col <- enquo(col)
  col_name <- paste0(quo_name(col), "_val")
  new_df <- df %>%
    mutate(!!col_name := str_split(!!(col), ";")) %>%
    unnest() %>%
    mutate(!!col_name := str_trim(!!as.name(col_name)))
  if(other_details == TRUE){
  new_df <- new_df %>%
    mutate(other_details = ifelse(str_detect(!!as.name(col_name), "[Oo]ther"), str_match(!!as.name(col_name), "[Oo]ther(.*)")[,2], NA)) %>%
    mutate(!!col_name := ifelse(str_detect(!!as.name(col_name), "[Oo]ther"), "other", !!as.name(col_name))) %>%
    mutate(other_details = str_replace_all(other_details, "([\\-_/*])", " "))
  }
  return(new_df)
}

#' Expand multiple-response fields into wide data form
#'
#' @param df The data frame containing the field to expand
#' @param col The column name to split into wide form
#' @param clean_names Whether new wide field names should be cleaned. Defaults to TRUE.
#'
#' @importFrom tidyr %>% unnest spread
#' @importFrom dplyr mutate
#' @importFrom stringr str_split str_trim
#' @importFrom janitor clean_names
#' @importFrom rlang enquo quo_name !!
#'
#' @return A wide form dataframe with `col` field separated into different columns and filled with TRUE or FALSE values.
#' @export
#' @rdname ed2_expand_wide
#' @examples
#' \dontrun{
#' humans <- ed2_human()
#' humans_reduced <- dplyr::select(humans, event_name, participant_id, travel_reason)
#' humans_reduced_wide <- ed2_expand_wide(humans_reduced, travel_reason)
#' }
ed2_expand_wide <- function(df, col, clean_names = TRUE){
  col <- enquo(col)
  name_stub <- paste0(quo_name(col))
  new_df <- df %>%
    mutate(cat_split = str_split(!!(col), ";")) %>%
    unnest() %>%
    mutate(cat_split = str_trim(cat_split)) %>%
    mutate(cat_split = if_else(str_detect(cat_split, "[Oo]ther"),"other",cat_split)) %>%
    mutate(cat_split = if_else(cat_split == "", "N/A", paste0(name_stub, "_", cat_split)), present = TRUE) %>%
    spread(cat_split, present, fill=FALSE)
  if(clean_names == TRUE){
    new_df <- clean_names(new_df)
  }
  return(new_df)
}

