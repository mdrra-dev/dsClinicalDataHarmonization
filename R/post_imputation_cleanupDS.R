#' @title Drop rows still incomplete after imputation
#' @param df A data frame (already resolved).
#' @param newobj Name under which the result is stored.
#' @return \code{list(newobj=, n_before=, n_after=, n_removed=)}.
#' @export
post_imputation_cleanupDS <- function(df, newobj) {
  n_before <- nrow(df)
  keep <- stats::complete.cases(df)
  df_clean <- df[keep, , drop = FALSE]
  base::assign(newobj, df_clean, envir = parent.frame())
  list(newobj = newobj, n_before = n_before, n_after = nrow(df_clean), n_removed = n_before - nrow(df_clean))
}

#' @title Count remaining NAs per column, without dropping anything
#' @param df A data frame (already resolved).
#' @return Named numeric vector of remaining NA counts (only columns with >0).
#' @export
check_residual_naDS <- function(df) {
  na_counts <- sapply(df, function(col) sum(is.na(col)))
  na_counts[na_counts > 0]
}
