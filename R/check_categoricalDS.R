#' @title Check validity of categorical variables
#' @description Verify that selected categorical variables in a data frame
#'   contain only predefined valid values.
#'
#' @param df A data frame containing the categorical variables to be checked.
#'
#' @return A character vector with the names of columns containing invalid values.
#'   Returns an empty character vector if all checked variables are valid.
#' @export

check_categoricalDS <- function(df) {

  normalize_missing <- function(x) {
    x[x == "" & !is.na(x)] <- NA
    x
  }

  valid_values <- list(
    Sex = c(0, 1, NA),
    RF_positivity = c(0, 1, NA),
    anti_CCP = c(0, 1, NA),
    GC = c(1, NA),
    GC_type = c(1, 2, 3, 4, NA),
    csDMARD1 = c(1, 2, 3, 4, 5, NA),
    csDMARD2 = c(1, 2, 3, 4, 5, NA),
    csDMARD3 = c(1, 2, 3, 4, 5, NA),
    # bDMARD: anti-TNF=1, anti-IL6=2, rituximab=3, abatacept=4, anti-IL1=5
    # (previous version of this check only allowed 1-4, silently flagging
    # every legitimate anti-IL1 record as invalid)
    bDMARD = c(1, 2, 3, 4, 5, NA),
    # tsDMARD: tofacitinib=1, baricitinib=2, upadacitinib=3, filgotinib=4
    tsDMARD = c(1, 2, 3, 4, NA),
    D2T = c(0, 1, NA)
  )

  invalid_cols <- c()
  for (col_name in names(valid_values)) {
    if (col_name %in% names(df)) {
      x <- normalize_missing(df[[col_name]])
      has_invalid <- any(!x %in% valid_values[[col_name]])
      if (has_invalid) invalid_cols <- c(invalid_cols, col_name)
    }
  }
  return(invalid_cols)
}
