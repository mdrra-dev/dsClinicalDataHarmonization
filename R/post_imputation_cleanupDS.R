#' @title Drop rows that are still incomplete after imputation
#' @description Imputation can legitimately leave some rows with residual
#'   \code{NA}s. This function lets the analyst explicitly drop them after
#'   seeing how many rows are affected (via \code{check_residual_naDS}); it
#'   never happens implicitly inside the imputation step itself.
#'   Disclosure-safe: the result is stored server-side via
#'   \code{base::(newobj, ..., envir = parent.frame())} and never appears in
#'   this function's own return value -- only row counts are.
#'
#' @param df A data frame (typically the output of an imputation step).
#' @param newobj Name under which the resulting data frame is stored.
#' @return A list with \code{newobj}, \code{n_before}, \code{n_after},
#'   \code{n_removed}.
#' @export
post_imputation_cleanupDS <- function(df, newobj = "post_imputation_cleanup_result") {

  n_before <- nrow(df)
  keep <- stats::complete.cases(df)
  df_clean <- df[keep, , drop = FALSE]

  base::assign(newobj, df_clean, envir = parent.frame())

  list(newobj = newobj, n_before = n_before, n_after = nrow(df_clean),
       n_removed = n_before - nrow(df_clean))
}

#' @title Count remaining NAs per column, without dropping anything
#' @description Lets the analyst inspect residual missingness after
#'   imputation before deciding whether to call
#'   \code{post_imputation_cleanupDS}. Never creates a server-side object,
#'   so this one is already a plain, disclosure-safe aggregate function --
#'   only per-column counts are returned, no row-level data.
#'
#' @param df A data frame.
#' @return A named numeric vector of remaining NA counts per column (only
#'   columns with at least one remaining NA are included).
#' @export
check_residual_naDS <- function(df) {
  na_counts <- sapply(df, function(col) sum(is.na(col)))
  na_counts[na_counts > 0]
}
