#' @title Value-set checks for categorical clinical variables (core logic)
#' @export
.cdh_check_categorical_core <- function(df) {
  invalid_cols <- character(0)

  valid_values <- list(
    Sex = c(0, 1, NA),
    RF_positivity = c(0, 1, NA),
    anti_CCP = c(0, 1, NA),
    csDMARD1 = c(1, 2, 3, 4, 5, NA),
    csDMARD2 = c(1, 2, 3, 4, 5, NA),
    csDMARD3 = c(1, 2, 3, 4, 5, NA),
    # bDMARD: anti-TNF=1, anti-IL6=2, rituximab=3, abatacept=4, anti-IL1=5
    bDMARD = c(1, 2, 3, 4, 5, NA),
    # tsDMARD: tofacitinib=1, baricitinib=2, upadacitinib=3, filgotinib=4
    tsDMARD = c(1, 2, 3, 4, NA),
    D2T = c(0, 1, NA),
    GC = c(1, NA),
    GC_type = c(1, 2, 3, 4, NA)
  )

  for (cn in names(valid_values)) {
    if (!cn %in% names(df)) next
    x <- suppressWarnings(as.numeric(as.character(df[[cn]])))
    x <- x[!is.na(x)]
    if (length(x) == 0) next
    allowed <- valid_values[[cn]][!is.na(valid_values[[cn]])]
    if (!all(x %in% allowed)) invalid_cols <- c(invalid_cols, cn)
  }

  invalid_cols
}

#' @title Value-set checks for categorical clinical variables
#' @param df A character string giving the name of the server-side data
#'   frame to check.
#' @return Character vector of column names with values outside their
#'   permitted set.
#' @export
check_categoricalDS <- function(df) {
  .cdh_check_categorical_core(df)
}
