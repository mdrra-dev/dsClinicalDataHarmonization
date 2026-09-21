#' @title Check patient-visit combinations for duplicates (core logic)
#' @export
.cdh_check_patient_visit_core <- function(df, pat_id_col = "pat_ID",
                                           visit_col = "Visit_months_from_diagnosis") {
  library(dplyr)

  if (!pat_id_col %in% names(df)) stop("pat_id_col '", pat_id_col, "' not found in data")
  if (!visit_col %in% names(df)) stop("visit_col '", visit_col, "' not found in data")

  pat_visit_counts <- df %>%
    dplyr::group_by(.data[[pat_id_col]], .data[[visit_col]]) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop")

  list(
    n_unique_patients = length(unique(df[[pat_id_col]])),
    n_unique_pairs    = nrow(pat_visit_counts),
    n_duplicate_pairs = sum(pat_visit_counts$n > 1),
    max_occurrences   = if (nrow(pat_visit_counts)) max(pat_visit_counts$n) else 0L
  )
}

#' @title Check patient-visit combinations for duplicates
#' @description Groups by patient identifier and visit column and checks for
#'   duplicate (patient, visit) combinations. Disclosure-safe: only aggregate
#'   counts are ever returned -- no patient identifier or row-level value.
#'
#' @param df A character string giving the name of the server-side data
#'   frame containing at least the columns named by \code{pat_id_col} and
#'   \code{visit_col}.
#' @param pat_id_col Character. Name of the patient identifier column.
#'   Default \code{"pat_ID"}.
#' @param visit_col Character. Name of the visit column. Default
#'   \code{"Visit_months_from_diagnosis"}.
#'
#' @return A list with \code{n_unique_patients}, \code{n_unique_pairs},
#'   \code{n_duplicate_pairs}, \code{max_occurrences}.
#' @export
check_patient_visitDS <- function(df, pat_id_col = "pat_ID",
                                   visit_col = "Visit_months_from_diagnosis") {
  .cdh_check_patient_visit_core(df, pat_id_col = pat_id_col, visit_col = visit_col)
}
