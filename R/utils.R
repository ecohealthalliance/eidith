camel_to_snake <- function(CamelNames) {
  Snake__names <- gsub("([a-z])([A-Z])", "\\1_\\L\\2", CamelNames, perl = TRUE)
  snake__names <- tolower(Snake__names)
  snake_names <- gsub(" ", "_", snake__names, fixed=TRUE)
  snake_names <- gsub("__", "_", snake_names, fixed=TRUE)
  return(snake_names)
  }
