#' @title Check validity of numeric variables
#' @description Verify that selected numeric variables in a data frame
#'   fall within predefined ranges and satisfy integer constraints where required.
#'   Also checks the format of the patient identifier (pat_ID), if present.
#'
#' @param df A data frame containing the numeric variables to be checked.
#'
#' @return A character vector with the names of columns containing invalid values.
#'   Returns an empty character vector if all checked variables are valid.
#' @export

check_numericDS <- function(df) {

  normalize_missing <- function(x) {
    x[x == "" & !is.na(x)] <- NA
    x
  }

  check_range <- function(x, min = NULL, max = NULL, integer = FALSE, decimals = NULL) {
    x <- normalize_missing(x)
    x_num <- suppressWarnings(as.numeric(x))  # se x era character, la converte

    invalid <- rep(FALSE, length(x_num))
    invalid <- invalid | (!is.na(x) & is.na(x_num))

    if (!is.null(min))
      invalid <- invalid | (!is.na(x_num) & x_num < min)
    if (!is.null(max))
      invalid <- invalid | (!is.na(x_num) & x_num > max)
    if (integer)
      invalid <- invalid | (!is.na(x_num) & x_num != round(x_num))
    if (!is.null(decimals))
      invalid <- invalid | (!is.na(x_num) & abs(x_num - round(x_num, decimals)) > .Machine$double.eps^0.5)
    any(invalid)
  }

  # NOTE ON month_diagnosis: the harmonization data dictionary specifies a
  # valid range of "0 to 12" (rather than the calendar-conventional 1-12).
  # That is followed literally here since it is the analyst-supplied
  # specification; if a future version of the dictionary tightens this to
  # 1-12, update min below accordingly.
  range_checks <- list(
    Visit_months_from_diagnosis = list(min = 0, decimals = 2),
    Age_diagnosis = list(min = 18, max = 110, integer = TRUE),
    DAS28 = list(min = 0, decimals = 2),
    Pat_global = list(min = 0, max = 100),
    Pain = list(min = 0, max = 100),
    Ph_global= list(min = 0, max = 100),
    CRP = list(min = 0, decimals = 1),
    ESR = list(min = 0, integer = TRUE),
    SJC28 = list(min = 0, max = 28, integer = TRUE),
    TJC28 = list(min = 0, max = 28, integer = TRUE),
    conc_MTX_dose = list(min = 0, decimals = 1),
    N_prev_csDMARD = list(min = 0, integer = TRUE),
    N_prev_bDMARD = list(min = 0, integer = TRUE),
    N_prev_tsDMARD = list(min = 0, integer = TRUE),
    GC_dose = list(min = 0, decimals = 1),
    eq5d = list(min = 0),
    HAQ = list(min = 0, max = 3),
    Year_diagnosis = list(min = 2010, integer = TRUE),
    month_diagnosis = list(min = 0, max = 12, integer = TRUE),
    Symptom_duration = list(min = 0, decimals = 1),
    Visit = list(min = 1, integer = TRUE),
    D2T = list(min = 0, max = 1, integer = TRUE),
    # ---- optional variables ----
    DAS28_CRP = list(min = 0, decimals = 2),
    USGS = list(min = 0),
    USPD = list(min = 0),
    NUSI = list(min = 0),
    N_comorbidities = list(min = 0, integer = TRUE),
    IP_score = list(min = 0, max = 4, integer = TRUE),
    EP_score = list(min = 0, max = 4, integer = TRUE),
    # ---- derived variables (computed by compute_derived_variablesDS(),
    # still worth range-checking once present) ----
    USPDGS = list(min = 0),
    DAS2C = list(min = 0)
  )

  invalid_cols <- c()
  for (col_name in names(range_checks)) {
    if (col_name %in% names(df)) {
      params <- range_checks[[col_name]]
      has_invalid <- do.call(check_range, c(list(x = df[[col_name]]), params))
      if (has_invalid) invalid_cols <- c(invalid_cols, col_name)
    }
  }

  if ("pat_ID" %in% names(df)) {
    pid <- normalize_missing(df$pat_ID)
    invalid_pid <- is.na(pid) | !grepl("^SE[0-9]+$", pid)
    if (any(invalid_pid)) invalid_cols <- c(invalid_cols, "pat_ID")
  }

  return(invalid_cols)
}
