#' @title Cast "NA" strings into type NA
#' @description Converts "NA" strings to proper NA values. Stores the result
#'   under \code{newobj} via \code{base::assign(newobj, ..., envir =
#'   parent.frame())} and returns only a small summary -- called via
#'   \code{datashield.aggregate}, never \code{datashield.assign.expr}.
#' @param df A data frame (already resolved).
#' @param newobj Name under which the result is stored in the session.
#' @return \code{list(newobj=, changed=)}.
#' @export
cast_NADS <- function(df, newobj) {
  changed <- FALSE
  df[] <- lapply(df, function(x) {
    if (is.character(x)) {
      if (any(x == "NA", na.rm = TRUE)) changed <<- TRUE
      x[x == "NA"] <- NA
    }
    x
  })
  base::assign(newobj, df, envir = parent.frame())
  list(newobj = newobj, changed = changed)
}
