#' @title Remove specified columns
#' @description Drops one or more named columns from a data frame.
#'   Disclosure-safe: the resulting data frame is stored server-side via
#'   \code{assign(newobj, ..., envir = parent.frame())} and never appears in
#'   this function's own return value.
#'
#' @param df A data frame.
#' @param col_names A character vector (or "$"-joined string) of column
#'   names to remove.
#' @param newobj Name under which the resulting data frame is stored.
#'
#' @return A list with \code{newobj}, \code{columns_removed} (subset of the
#'   requested columns that were actually present), \code{n_cols_before},
#'   and \code{n_cols_after}.
#' @export
remove_columnsDS <- function(df, col_names, newobj = "remove_columns_result") {

  columns <- intersect(cdh_split_cols(col_names), names(df))
  n_cols_before <- ncol(df)

  df_clean <- df[, !(names(df) %in% columns), drop = FALSE]

  assign(newobj, df_clean, envir = parent.frame())

  list(newobj = newobj, columns_removed = columns,
       n_cols_before = n_cols_before, n_cols_after = ncol(df_clean))
}
