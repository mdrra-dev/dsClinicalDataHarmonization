#' @title Drop rows with missing values in specified columns
#' @param df A data frame (already resolved).
#' @param col_names A character vector (or "$"-joined string) of columns to check.
#' @param newobj Name under which the result is stored.
#' @return \code{list(newobj=, n_before=, n_after=, n_removed=)}.
#' @export
drop_rows_missingDS <- function(df, col_names, newobj) {
  cols <- intersect(cdh_split_cols(col_names), names(df))
  n_before <- nrow(df)
  keep <- stats::complete.cases(df[, cols, drop = FALSE])
  df_clean <- df[keep, , drop = FALSE]
  base::assign(newobj, df_clean, envir = parent.frame())
  list(newobj = newobj, n_before = n_before, n_after = nrow(df_clean),
       n_removed = n_before - nrow(df_clean))
}
