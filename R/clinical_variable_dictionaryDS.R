#' @title Canonical clinical data dictionary for the harmonization module
#' @description Single source of truth for which variables the RA harmonization
#'   protocol expects, split into \code{required} (must be present in every
#'   site's table) and \code{optional} (checked for valid range/type only if
#'   present -- a site is never flagged for omitting them). This is what
#'   \code{ds.check_variables()} defaults to when the analyst does not supply
#'   an explicit variable list, and what \code{check_numericDS()} /
#'   \code{check_categoricalDS()} validate against.
#'
#' @return A named list with two character vectors: \code{required} and
#'   \code{optional}.
#' @export
clinical_variable_dictionaryDS <- function() {
  list(
    required = c(
      "pat_ID", "Visit_months_from_diagnosis", "Age_diagnosis", "Sex",
      "RF_positivity", "anti_CCP", "DAS28", "Pat_global", "Pain",
      "Ph_global", "CRP", "ESR", "SJC28", "TJC28",
      "csDMARD1", "csDMARD2", "csDMARD3", "conc_MTX_dose",
      "N_prev_csDMARD", "bDMARD", "N_prev_bDMARD", "tsDMARD",
      "N_prev_tsDMARD", "GC", "GC_type", "GC_dose", "eq5d", "HAQ",
      "Year_diagnosis", "month_diagnosis", "Symptom_duration", "D2T", "Visit"
    ),
    optional = c(
      "DAS28_CRP", "USGS", "USPD", "NUSI", "N_comorbidities",
      "Comorbidities", "IP_score", "EP_score"
    )
  )
}
