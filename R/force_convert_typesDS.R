#' @title Detect columns whose stored type doesn't match the expected type (dry run)
#' @param df A data frame (already resolved).
#' @param numeric_cols,categorical_cols Character vectors (or "$"-joined strings).
#' @return \code{list(numeric_would_change=, categorical_would_change=)}.
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
#' @param df A data frame (already resolved).
#' @param numeric_cols,categorical_cols Character vectors (or "$"-joined strings).
#' @param newobj Name under which the result is stored.
#' @param zero_prop_threshold,spike_ratio_threshold Passed to \code{detect_zero_anomaliesDS}
#'   for the automatic post-conversion check on \code{numeric_cols}.
#' @return \code{list(newobj=, numeric_converted=, categorical_converted=, zero_anomalies=)}.
#' @export
force_convert_typesDS <- function(df, numeric_cols = NULL, categorical_cols = NULL, newobj,
                                   zero_prop_threshold = 0.3, spike_ratio_threshold = 3) {
  numeric_converted <- character(0); categorical_converted <- character(0)

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
    zero_anomalies <- detect_zero_anomaliesDS(df, cols = numeric_converted,
      zero_prop_threshold = zero_prop_threshold, spike_ratio_threshold = spike_ratio_threshold)
  }

  list(newobj = newobj, numeric_converted = numeric_converted,
       categorical_converted = categorical_converted, zero_anomalies = zero_anomalies)
}
