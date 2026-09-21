#' @title Remove specified columns
#' @param df A data frame (already resolved).
#' @param col_names A character vector (or "$"-joined string) of columns to remove.
#' @param newobj Name under which the result is stored.
#' @return \code{list(newobj=, columns_removed=, n_cols_before=, n_cols_after=)}.
#' @export
remove_columnsDS <- function(df, col_names, newobj) {
  columns <- intersect(cdh_split_cols(col_names), names(df))
  n_cols_before <- ncol(df)
  df_clean <- df[, !(names(df) %in% columns), drop = FALSE]
  base::assign(newobj, df_clean, envir = parent.frame())
  list(newobj = newobj, columns_removed = columns,
       n_cols_before = n_cols_before, n_cols_after = ncol(df_clean))
}
