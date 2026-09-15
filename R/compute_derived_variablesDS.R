#' @title Compute derived clinical variables
#' @description Derives two composite variables when their source columns are
#'   available, and leaves the data frame otherwise untouched -- some sites
#'   may not collect ultrasound data at all, and this should not block the
#'   rest of the harmonization pipeline.
#'   \itemize{
#'     \item \code{USPDGS = USPD + USGS}, computed only if both \code{USPD}
#'       and \code{USGS} are present.
#'     \item \code{DAS2C = sqrt(SJC28) + 0.6 * log1p(CRP)}, computed only if
#'       both \code{SJC28} and \code{CRP} are present.
#'   }
#'   Row-wise: if either input is \code{NA} for a given row, the derived
#'   value for that row is \code{NA} too. Disclosure-safe: the result is
#'   stored server-side via \code{base::assign(newobj, ..., envir = parent.frame())}
#'   and never appears in this function's own return value.
#'
#' @param df A data frame.
#' @param newobj Name under which the resulting data frame is stored.
#' @return A list with \code{newobj} and \code{computed} (character vector of
#'   which derived columns were actually added -- column NAMES only).
#' @export
compute_derived_variablesDS <- function(df, newobj = "compute_derived_variables_result") {

  computed <- c()

  if (all(c("USPD", "USGS") %in% names(df))) {
    df$USPDGS <- df$USPD + df$USGS
    computed <- c(computed, "USPDGS")
  }

  if (all(c("SJC28", "CRP") %in% names(df))) {
    df$DAS2C <- sqrt(df$SJC28) + 0.6 * log1p(df$CRP)
    computed <- c(computed, "DAS2C")
  }

  base::assign(newobj, df, envir = parent.frame())

  list(newobj = newobj, computed = computed)
}
