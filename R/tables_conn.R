#' Connecting eidith tables
#'
#' To understand how various eidith tables are connected via their column names.
#' Visualizing these connections is helpful in determining how to join
#' `eidith` tables for further analyses.
#'
#' @param cols can be either `all` or `id`. If `all`, all common columns are
#' presented. If `id`, only unique id columns are shown.
#' Default value of `type` is `id`.
#'@return a data frame with three columns: `T1` and `T2` corresponding to eidith
#' tables, and `conn`, the connecting column. 
#' @export
#'
#' 
#' @examples
#' ed_tables_conn()
#' ed_tables_conn(cols = "all")
#'
#'

ed_tables_conn <- function(cols = "id"){

    df <- ed_metadata()

    # Selecting those names that haven't been dropped
    df <- df[ !df$replacement_name %in% "DROP", ]

    # Using replacement names wherever applicable
    rep <- df$replacement_name[ !is.na(df$replacement_name) ]
    df$auto_processed_name[ !is.na(df$replacement_name) ] <- rep

    # Removing columns not relevant to this function
    df <- df[ , c("endpoint", "auto_processed_name", "replacement_name")]

    # Selecting names which occur in more than 1 table
    dup <- unique(df$auto_processed_name[duplicated(df$auto_processed_name)])

    # Selecting rows corresponding to above names
    conn <- df[ df$auto_processed_name %in% dup, ]

    # Generating connection table
    conn <- conn[ order(conn$auto_processed_name), ]

    conn_l <- split(conn$endpoint, conn$auto_processed_name)

    tbl_conn <- lapply(1:length(conn_l), function(x, l) {
        df <- l[[x]]
        conn_name <- names(l)[x]
        df <- data.frame(expand.grid.unique(df, df), stringsAsFactors = FALSE)
        colnames(df) <- c("T1", "T2")
        df$conn <- conn_name
        df
    }, conn_l)


    tbl_conn <- do.call(rbind, tbl_conn)

    od1 <- names(sort(table(tbl_conn$T1), decreasing = TRUE))

    tbl_conn$T1 <- factor(tbl_conn$T1, levels = od1)

    T2_names <- names(sort(table(tbl_conn$T2)))

    T2_in_T1 <- T2_names %in% od1
    od2 <- c(T2_names[ T2_in_T1 ], T2_names[ !T2_in_T1 ])

    tbl_conn$T2 <- factor(tbl_conn$T2, levels = od2)
    tbl_conn <- tbl_conn[ order(tbl_conn$T1, tbl_conn$T2), ]
    rownames(tbl_conn) <- NULL

    if(cols == "all") {
        return(tbl_conn)
    } else {
        r <- tbl_conn[ grepl("id", tbl_conn$conn), ]
        rownames(r) = NULL
        return(r)
    }
}



expand.grid.unique <- function(x, y, include.equals=FALSE) {
    x <- unique(x)
    y <- unique(y)
    g <- function(i) {
        z <- setdiff(y, x[seq_len(i-include.equals)])
        if(length(z)) cbind(x[i], z, deparse.level=0)
    }
    do.call(rbind, lapply(seq_along(x), g))
}
