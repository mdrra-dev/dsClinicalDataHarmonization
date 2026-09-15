#' @title Cast specified columns to factor
#' @description Converts one or more columns of a data frame to factor.
#'   Disclosure-safe: the modified data frame is stored server-side via
#'   \code{base::assign(newobj, ..., envir = parent.frame())} and never appears in
#'   this function's own return value.
#'
#' @param df A data frame.
#' @param columns A character vector (or "$"-joined string) of column names
#'   to convert to factor.
#' @param newobj Name under which the modified data frame is stored.
#'
#' @return A list with \code{newobj} and \code{columns_cast} (the subset of
#'   requested columns that were actually present and converted).
#' @export
cast_to_factorDS <- function(df, columns, newobj = "cast_to_factor_result") {

  cols <- intersect(cdh_split_cols(columns), names(df))
  df[cols] <- lapply(df[cols], as.factor)

  base::assign(newobj, df, envir = parent.frame())

  list(newobj = newobj, columns_cast = cols)
}
