#' @title Cast specified columns to factor
#' @param df A data frame (already resolved).
#' @param columns A character vector (or "$"-joined string) of columns.
#' @param newobj Name under which the result is stored.
#' @return \code{list(newobj=, columns_cast=)}.
#' @export
cast_to_factorDS <- function(df, columns, newobj) {
  cols <- intersect(cdh_split_cols(columns), names(df))
  df[cols] <- lapply(df[cols], as.factor)
  base::assign(newobj, df, envir = parent.frame())
  list(newobj = newobj, columns_cast = cols)
}
