#' @title Dummy-encode ordinal/nominal-coded variables
#' @description For each named column, coerces it to integer (keeping the
#'   ORIGINAL column, unlike \code{cast_to_factorDS} which replaces it) and
#'   adds one integer 0/1 dummy column per observed level, named
#'   \code{"<column>_<level>"}. Rows with \code{NA} in the source column get
#'   \code{NA} in every dummy for that column.
#'
#'   \code{IP_score} and \code{EP_score} are explicitly excluded even if
#'   passed in \code{columns} -- these are patient-annotation severity scores
#'   (0-4), not treatment-class categories.
#'
#'   Disclosure-safe: the result is stored server-side via
#'   \code{base::assign(newobj, ..., envir = parent.frame())} and never appears in
#'   this function's own return value -- only column NAMES are returned.
#'
#' @param df A data frame.
#' @param columns A character vector (or "$"-joined string) of columns to
#'   dummy-encode.
#' @param newobj Name under which the resulting data frame is stored.
#'
#' @return A list with \code{newobj}, \code{added} (new dummy column names),
#'   and \code{skipped_excluded} (requested columns skipped because they're
#'   on the excluded list).
#' @export
dummy_encode_ordinalDS <- function(df, columns, newobj = "dummy_encode_ordinal_result") {

  cols <- cdh_split_cols(columns)

  excluded <- c("IP_score", "EP_score")
  skipped_excluded <- intersect(cols, excluded)
  cols <- setdiff(cols, excluded)
  cols <- intersect(cols, names(df))

  dummy_cols_added <- c()

  for (col in cols) {
    x_int <- suppressWarnings(as.integer(as.character(df[[col]])))
    df[[col]] <- x_int

    levels_present <- sort(unique(x_int[!is.na(x_int)]))
    for (lv in levels_present) {
      dname <- paste0(col, ".", lv)
      dummy <- ifelse(is.na(x_int), NA_integer_, as.integer(x_int == lv))
      df[[dname]] <- dummy
      dummy_cols_added <- c(dummy_cols_added, dname)
    }
  }

  base::assign(newobj, df, envir = parent.frame())

  list(newobj = newobj, added = dummy_cols_added, skipped_excluded = skipped_excluded)
}
