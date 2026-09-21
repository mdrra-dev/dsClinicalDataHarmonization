#' @title Compute percentage of missing values per column (core logic)
#' @export
.cdh_check_missing_data_core <- function(df) {
  if (!is.data.frame(df)) {
    stop("The input table must be a data frame.")
  }
  sapply(df, function(col) sum(is.na(col)) / length(col) * 100)
}

#' @title Compute percentage of missing values per column
#'
#' @description Computes the proportion of missing values (NA) for each column of a data frame.
#' The function returns a named numeric vector containing the percentage of
#' missing values for every variable.
#'
#' @param df A character string giving the name of the server-side data
#'   frame for which the percentage of missing values per column is to be
#'   calculated.
#'
#' @return A named numeric vector where each element represents the percentage
#' of missing values in the corresponding column of \code{df}.
#'
#' @export
check_missing_dataDS <- function(df) {
  .cdh_check_missing_data_core(df)
}
