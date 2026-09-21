#' @title Range/format checks for numeric clinical variables (core logic)
#' @export
.cdh_check_numeric_core <- function(df) {
  invalid_cols <- character(0)

  if ("pat_ID" %in% names(df)) {
    x <- df[["pat_ID"]]
    x <- x[!is.na(x)]
    if (length(x) > 0 && !all(grepl("^SE[0-9]+$", as.character(x)))) {
      invalid_cols <- c(invalid_cols, "pat_ID")
    }
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

  for (cn in names(range_checks)) {
    if (!cn %in% names(df)) next
    x <- suppressWarnings(as.numeric(df[[cn]]))
    x <- x[!is.na(x)]
    if (length(x) == 0) next

    spec <- range_checks[[cn]]
    ok <- TRUE
    if (!is.null(spec$min) && any(x < spec$min)) ok <- FALSE
    if (!is.null(spec$max) && any(x > spec$max)) ok <- FALSE
    if (isTRUE(spec$integer) && any(x != round(x))) ok <- FALSE
    if (!is.null(spec$decimals)) {
      mult <- 10 ^ spec$decimals
      if (any(round(x * mult) != x * mult)) ok <- FALSE
    }
    if (!ok) invalid_cols <- c(invalid_cols, cn)
  }

  invalid_cols
}

#' @title Range/format checks for numeric clinical variables
#' @param df A character string giving the name of the server-side data
#'   frame to check.
#' @return Character vector of column names failing their range/format check.
#' @export
check_numericDS <- function(df) {
  .cdh_check_numeric_core(df)
}
