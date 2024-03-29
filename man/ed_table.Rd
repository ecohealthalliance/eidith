% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tables.R
\name{ed_table_}
\alias{ed_table_}
\alias{ed_table}
\alias{ed_events}
\alias{ed_animals}
\alias{ed_specimens}
\alias{ed_tests}
\alias{ed_testspecimen}
\title{Load PREDICT-1 EIDITH data from the local database}
\usage{
ed_table_(table, ..., .dots)

ed_table(table, ...)

ed_events(...)

ed_animals(...)

ed_specimens(...)

ed_tests(...)

ed_testspecimen(...)
}
\arguments{
\item{table}{one of the EIDITH PREDICT-1 or PREDICT-2 database tables.}

\item{...}{arguments passed to \code{\link[dplyr:filter]{dplyr::filter()}} to subset data}

\item{.dots}{standard-evaluation versions of subsetting arguments}
}
\value{
a \link[tibble:tibble]{tibble}-style data frame.
}
\description{
These functions load data from the locally-stored SQLite database of downloaded
and cleaned EIDITH data. \code{ed_table} is a general function, and \code{ed_table_}
it's \href{https://cran.r-project.org/web/packages/dplyr/vignettes/nse.html}{standard evaluation}
equivalent, useful for programming.  The other functions are convenience aliases
for the individual tables.
Alternate versions return \link{mock data} `ed_mock()`` for
tutorials and practice.
}
\details{
These functions take \code{\link[dplyr:filter]{dplyr::filter()}} arguments to sub-set the data.  Using
these, the data is subsetted via SQL \emph{before} it is loaded into memory.
For large tables, such as the \emph{tests} table, this is useful for reducing the memory footprint of your R session.

Note that subsetting in SQL is more limited:
\itemize{
\item Use \code{0} or \code{1} instead of \code{TRUE} or \code{FALSE}
\item Dates are stored as character strings, but as they are in YYYY-MM-DD
format, filtering such as \code{event_date > "2014-01-01"} still works.
}
}
