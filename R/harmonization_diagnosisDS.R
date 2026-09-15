#' @title Consolidated, non-disclosive harmonization diagnosis
#' @description Bundles every existing check in this package
#'   (\code{check_variablesDS}, \code{check_numericDS}, \code{check_categoricalDS},
#'   \code{check_missing_dataDS}, \code{check_patient_visitDS},
#'   \code{detect_zero_anomaliesDS}) plus a check for "NA"-string
#'   contamination into a single aggregate call, so a full site diagnosis
#'   takes one round trip instead of six. Used by
#'   \code{ds.harmonization_orchestrator()} for both the "before" and "after"
#'   snapshots, but also usable standalone.
#'
#' @param df A data frame.
#' @param required_vars,optional_vars Character vectors (or "$"-joined
#'   strings) -- see \code{check_variablesDS}.
#' @param pat_id_col,visit_col See \code{check_patient_visitDS}. Duplicate
#'   checking is skipped (returns \code{NA}) if either column is absent.
#' @param nfilter Disclosure/stability floor, passed through to every
#'   sub-check that has one.
#' @param zero_prop_threshold,spike_ratio_threshold See
#'   \code{detect_zero_anomaliesDS}.
#'
#' @return A list:
#'   \itemize{
#'     \item \code{n}: row count at this site.
#'     \item \code{missing_required}, \code{optional_present}, \code{extra_vars}:
#'       from \code{check_variablesDS}.
#'     \item \code{na_string_present}: logical, whether any character column
#'       contains the literal string \code{"NA"} (a sign \code{cast_NADS}
#'       hasn't been run yet).
#'     \item \code{invalid_numeric}, \code{invalid_categorical}: column names
#'       failing range/value checks.
#'     \item \code{missing_pct}: named numeric vector, \% missing per column.
#'     \item \code{duplicate_patient_visit_pairs}: count of patient-visit
#'       pairs occurring more than once, or \code{NA} if the id/visit columns
#'       aren't both present.
#'     \item \code{zero_anomalies}: output of \code{detect_zero_anomaliesDS}.
#'   }
#' @export
harmonization_diagnosisDS <- function(df, required_vars, optional_vars,
                                       pat_id_col = "pat_ID",
                                       visit_col = "Visit",
                                       nfilter = 5,
                                       zero_prop_threshold = 0.3,
                                       spike_ratio_threshold = 3) {

  n <- nrow(df)
  if (n < nfilter) stop("site n below disclosure threshold")

  var_check <- check_variablesDS(
    df,
    variables_string = paste(cdh_split_cols(required_vars), collapse = "$"),
    optional_string  = paste(cdh_split_cols(optional_vars), collapse = "$")
  )

  na_string_present <- any(vapply(df, function(x) {
    is.character(x) && any(x == "NA", na.rm = TRUE)
  }, logical(1)))

  invalid_numeric     <- check_numericDS(df)
  invalid_categorical <- check_categoricalDS(df)
  missing_pct         <- check_missing_dataDS(df)

  dup_count <- NA_integer_
  if (pat_id_col %in% names(df) && visit_col %in% names(df)) {
    pv <- check_patient_visitDS(df, pat_id_col = pat_id_col, visit_col = visit_col)
    dup_count <- pv$n_duplicate_pairs
  }

  zero_anomalies <- detect_zero_anomaliesDS(
    df, cols = NULL,
    zero_prop_threshold = zero_prop_threshold,
    spike_ratio_threshold = spike_ratio_threshold,
    nfilter = nfilter
  )

  list(
    n = n,
    missing_required = var_check$missing,
    optional_present = var_check$optional_present,
    extra_vars = var_check$extra,
    na_string_present = na_string_present,
    invalid_numeric = invalid_numeric,
    invalid_categorical = invalid_categorical,
    missing_pct = missing_pct,
    duplicate_patient_visit_pairs = dup_count,
    zero_anomalies = zero_anomalies
  )
}
