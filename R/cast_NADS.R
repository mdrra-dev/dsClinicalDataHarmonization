#' @title Cast "NA" strings into type NA
#' @description Converts "NA" strings of a data frame to proper NA values.
#'   Follows the package's disclosure-safe pattern: the transformed data
#'   frame is stored server-side via \code{base::assign(newobj, ..., envir =
#'   parent.frame())} and is NEVER part of this function's own return value
#'   -- so even if this function were ever invoked as an aggregate call by
#'   mistake, no row-level data crosses the wire. Only a small non-disclosive
#'   summary (whether any replacement occurred) is returned.
#'
#' @param df A data frame containing values to be converted.
#' @param newobj Name under which the converted data frame is stored on the
#'   server (in the calling environment, i.e. the analytic session).
#'
#' @return A list with \code{newobj} (the name just base::assigned) and
#'   \code{changed} (logical, whether any "NA" string was replaced).
#' @export
cast_NADS <- function(df, newobj = "cast_NA_result") {

  changed <- FALSE
  df[] <- lapply(df, function(x) {
    if (is.character(x)) {
      if (any(x == "NA", na.rm = TRUE) || any(x == "", na.rm = TRUE)) {
        changed <<- TRUE
      }
      x[x == "NA"] <- NA
      x[x == ""] <- NA
    }
    x
  })

  base::assign(newobj, df, envir = parent.frame())

  list(newobj = newobj, changed = changed)
}
