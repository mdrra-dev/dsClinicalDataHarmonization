#' @title Compute derived clinical variables
#' @description Computed only when source columns are available (missing
#'   sources = skipped, not an error):
#'   \itemize{
#'     \item \code{USPDGS = USPD + USGS}
#'     \item \code{DAS2C = sqrt(SJC28) + 0.6 * log(CRP + 1)}
#'     \item \code{CDAI50 = SJC28 + TJC28 + Pain/10 + Ph_global/10}
#'     \item \code{SDAI = CRP + CDAI50} (requires CDAI50 to have been computed)
#'   }
#' @param df A data frame (already resolved).
#' @param newobj Name under which the result is stored.
#' @return \code{list(newobj=, computed=)}.
#' @export
compute_derived_variablesDS <- function(df, newobj) {
  computed <- c()

  if (all(c("USPD", "USGS") %in% names(df))) {
    df$USPDGS <- df$USPD + df$USGS
    computed <- c(computed, "USPDGS")
  }
  if (all(c("SJC28", "CRP") %in% names(df))) {
    df$DAS2C <- sqrt(df$SJC28) + 0.6 * log(df$CRP + 1)
    computed <- c(computed, "DAS2C")
  }
  if (all(c("SJC28", "TJC28", "Pain", "Ph_global") %in% names(df))) {
    df$CDAI50 <- df$SJC28 + df$TJC28 + df$Pain / 10 + df$Ph_global / 10
    computed <- c(computed, "CDAI50")
  }
  if (all(c("CRP", "CDAI50") %in% names(df))) {
    df$SDAI <- df$CRP + df$CDAI50
    computed <- c(computed, "SDAI")
  }

  base::assign(newobj, df, envir = parent.frame())
  list(newobj = newobj, computed = computed)
}
