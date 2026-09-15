#' @title Drop rows with missing values in specified columns
#' @description Companion to \code{remove_columnsDS} for the missing-data
#'   workflow: instead of dropping a variable that exceeds the missingness
#'   threshold, this drops the ROWS that are missing on that variable,
#'   keeping the variable itself. Disclosure-safe: the resulting data frame
#'   is stored server-side via \code{base::assign(newobj, ..., envir =
#'   parent.frame())} and never appears in this function's own return value
#'   -- only row counts (aggregate, non-disclosive) are returned.
#'
#' @param df A data frame.
#' @param col_names A character vector (or "$"-joined string) of column
#'   names to check for missingness.
#' @param newobj Name under which the resulting data frame is stored.
#'
#' @return A list with \code{newobj}, \code{n_before}, \code{n_after},
#'   \code{n_removed}.
#' @export
drop_rows_missingDS <- function(df, col_names, newobj = "drop_rows_missing_result") {

  cols <- intersect(cdh_split_cols(col_names), names(df))

  n_before <- nrow(df)
  keep <- stats::complete.cases(df[, cols, drop = FALSE])
  df_clean <- df[keep, , drop = FALSE]

  base::assign(newobj, df_clean, envir = parent.frame())

  list(newobj = newobj, n_before = n_before, n_after = nrow(df_clean),
       n_removed = n_before - nrow(df_clean))
}
