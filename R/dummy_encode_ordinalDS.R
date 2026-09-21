#' @title Dummy-encode ordinal/nominal-coded variables
#' @description Original column becomes a FACTOR (excluded from numeric
#'   analysis, picked up by categorical checks); one integer 0/1 dummy per
#'   observed level is added, named \code{"<column>_<level>"}. \code{IP_score}
#'   and \code{EP_score} are always excluded.
#' @param df A data frame (already resolved).
#' @param columns A character vector (or "$"-joined string) of columns.
#' @param newobj Name under which the result is stored.
#' @return \code{list(newobj=, added=, skipped_excluded=)}.
#' @export
dummy_encode_ordinalDS <- function(df, columns, newobj) {
  cols <- cdh_split_cols(columns)
  excluded <- c("IP_score", "EP_score")
  skipped_excluded <- intersect(cols, excluded)
  cols <- intersect(setdiff(cols, excluded), names(df))

  dummy_cols_added <- c()
  for (col in cols) {
    x_int <- suppressWarnings(as.integer(as.character(df[[col]])))
    levels_present <- sort(unique(x_int[!is.na(x_int)]))
    for (lv in levels_present) {
      dname <- paste0(col, "_", lv)
      df[[dname]] <- ifelse(is.na(x_int), NA_integer_, as.integer(x_int == lv))
      dummy_cols_added <- c(dummy_cols_added, dname)
    }
    df[[col]] <- factor(x_int)
  }

  base::assign(newobj, df, envir = parent.frame())
  list(newobj = newobj, added = dummy_cols_added, skipped_excluded = skipped_excluded)
}
