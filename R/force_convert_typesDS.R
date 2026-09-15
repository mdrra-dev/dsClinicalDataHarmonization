#' @title Detect columns whose stored type doesn't match the expected type
#' @description Dry-run companion to \code{force_convert_typesDS} -- reports
#'   which columns WOULD change type if conversion were forced, without
#'   actually modifying anything. Never touches or returns any data value,
#'   only column names, so this one is safe as a plain aggregate return
#'   already (no server-side object is created).
#'
#' @param df A data frame.
#' @param numeric_cols Character vector (or "$"-joined string) of columns
#'   expected to be numeric.
#' @param categorical_cols Character vector (or "$"-joined string) of columns
#'   expected to be a factor.
#' @return A list with \code{numeric_would_change} and
#'   \code{categorical_would_change}.
#' @export
detect_type_mismatchesDS <- function(df, numeric_cols = NULL, categorical_cols = NULL) {

  out <- list(numeric_would_change = character(0), categorical_would_change = character(0))

  if (!is.null(numeric_cols)) {
    cols <- intersect(cdh_split_cols(numeric_cols), names(df))
    out$numeric_would_change <- cols[!vapply(df[cols], is.numeric, logical(1))]
  }

  if (!is.null(categorical_cols)) {
    cols <- intersect(cdh_split_cols(categorical_cols), names(df))
    out$categorical_would_change <- cols[!vapply(df[cols], is.factor, logical(1))]
  }

  out
}

#' @title Force type conversion on selected columns
#' @description Coerces the named columns to numeric and/or factor.
#'   Disclosure-safe: the result is stored server-side via
#'   \code{base::assign(newobj, ..., envir = parent.frame())} and never appears in
#'   this function's own return value -- only column names and, for the
#'   numeric columns just converted, non-disclosive zero-anomaly diagnostics
#'   (see \code{detect_zero_anomaliesDS}) are returned.
#'
#' @param df A data frame.
#' @param numeric_cols Character vector (or "$"-joined string) of columns to
#'   force to numeric.
#' @param categorical_cols Character vector (or "$"-joined string) of columns
#'   to force to factor.
#' @param newobj Name under which the resulting data frame is stored.
#' @param zero_prop_threshold,spike_ratio_threshold Passed through to
#'   \code{detect_zero_anomaliesDS} for the automatic post-conversion check
#'   on \code{numeric_cols}.
#' @return A list with \code{newobj}, \code{numeric_converted},
#'   \code{categorical_converted}, and \code{zero_anomalies} (diagnostics for
#'   the newly-converted numeric columns; empty list if none were converted).
#' @export
force_convert_typesDS <- function(df, numeric_cols = NULL, categorical_cols = NULL,
                                   newobj = "force_convert_types_result",
                                   zero_prop_threshold = 0.3, spike_ratio_threshold = 3) {

  numeric_converted <- character(0)
  categorical_converted <- character(0)

  if (!is.null(numeric_cols)) {
    numeric_converted <- intersect(cdh_split_cols(numeric_cols), names(df))
    df[numeric_converted] <- lapply(df[numeric_converted], function(x) suppressWarnings(as.numeric(as.character(x))))
  }

  if (!is.null(categorical_cols)) {
    categorical_converted <- intersect(cdh_split_cols(categorical_cols), names(df))
    df[categorical_converted] <- lapply(df[categorical_converted], as.factor)
  }

  base::assign(newobj, df, envir = parent.frame())

  zero_anomalies <- list()
  if (length(numeric_converted) > 0) {
    zero_anomalies <- detect_zero_anomaliesDS(
      df, cols = numeric_converted,
      zero_prop_threshold = zero_prop_threshold,
      spike_ratio_threshold = spike_ratio_threshold
    )
  }

  list(newobj = newobj, numeric_converted = numeric_converted,
       categorical_converted = categorical_converted, zero_anomalies = zero_anomalies)
}
