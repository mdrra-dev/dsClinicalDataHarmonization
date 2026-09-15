#' @title Detect suspicious zero-inflation in numeric columns
#' @description A forced or careless numeric cast can silently turn
#'   non-numeric placeholders into zeros instead of \code{NA} -- e.g. a
#'   factor coerced with \code{as.numeric(x)} instead of
#'   \code{as.numeric(as.character(x))} (returns internal factor level
#'   codes, not the labelled values), or an upstream ETL step that filled
#'   blanks with \code{0} before the column ever reached this pipeline. This
#'   failure mode is invisible to \code{check_missing_dataDS} (the cell isn't
#'   \code{NA}, it's a real-looking 0) and easy to miss visually in a
#'   clinical variable where a *few* genuine zeros are plausible (e.g. CRP,
#'   TJC28), so a purely eyeballed check tends to under-detect it.
#'
#'   Two independent, complementary flags are computed per numeric column:
#'   \itemize{
#'     \item \strong{Proportion flag}: the share of non-missing values that
#'       are exactly 0 exceeds \code{zero_prop_threshold}. Simple, but can
#'       miss a genuinely zero-inflated variable's true signal vs. a
#'       cast artefact, and can also flag a variable that is legitimately
#'       often zero (e.g. TJC28 in a well-controlled cohort).
#'     \item \strong{Spike flag}: zero-count is compared to the AVERAGE
#'       count across equal-width bins spanning the non-zero values --
#'       i.e. "does 0 stick out as a spike relative to the local density of
#'       the rest of the distribution", which is what an artefact typically
#'       looks like (a hard wall at exactly 0 that a smooth clinical
#'       distribution wouldn't produce) as opposed to a distribution that is
#'       just generally low-valued. Flagged when
#'       \code{n_zero / mean(bin_counts) >= spike_ratio_threshold}.
#'   }
#'   A column is reported as anomalous if EITHER flag fires. Neither flag is
#'   proof of an error -- both are heuristics meant to prompt a human look,
#'   not an automatic correction; this function never modifies data.
#'
#' @param df A data frame.
#' @param cols Character vector (or "$"-joined string) restricting the check
#'   to specific columns. \code{NULL} (default): check every numeric column.
#' @param zero_prop_threshold Proportion of exact zeros (of non-missing
#'   values) above which the proportion flag fires. Default 0.3.
#' @param spike_ratio_threshold Ratio of zero-count to average non-zero bin
#'   count above which the spike flag fires. Default 3.
#' @param num_bins Number of equal-width bins used for the spike check across
#'   the non-zero values. Default 10.
#' @param nfilter Minimum number of non-missing values required before a
#'   column is evaluated at all (disclosure/stability floor).
#'
#' @return A named list, one entry per checked numeric column with at least
#'   \code{nfilter} non-missing values, each containing \code{n_nonmissing},
#'   \code{n_zero}, \code{prop_zero}, \code{spike_ratio}, \code{prop_flag},
#'   \code{spike_flag}, \code{flagged} (either flag fired).
#' @export
detect_zero_anomaliesDS <- function(df, cols = NULL, zero_prop_threshold = 0.3,
                                     spike_ratio_threshold = 3, num_bins = 10,
                                     nfilter = 5) {

  candidate_cols <- if (is.null(cols)) {
    names(df)[sapply(df, is.numeric)]
  } else {
    intersect(cdh_split_cols(cols), names(df))
  }

  out <- lapply(candidate_cols, function(cn) {
    x <- df[[cn]]
    x <- x[!is.na(x)]
    n_nonmissing <- length(x)
    if (n_nonmissing < nfilter) return(NULL)

    n_zero <- sum(x == 0)
    prop_zero <- n_zero / n_nonmissing

    nonzero <- x[x != 0]
    spike_ratio <- NA_real_
    if (length(nonzero) >= 2 && diff(range(nonzero)) > 0) {
      breaks <- seq(min(nonzero), max(nonzero), length.out = num_bins + 1)
      bin_counts <- table(cut(nonzero, breaks = breaks, include.lowest = TRUE))
      avg_bin_count <- mean(bin_counts)
      spike_ratio <- if (avg_bin_count > 0) n_zero / avg_bin_count else NA_real_
    }

    prop_flag  <- prop_zero >= zero_prop_threshold
    spike_flag <- !is.na(spike_ratio) && spike_ratio >= spike_ratio_threshold

    list(
      n_nonmissing = n_nonmissing,
      n_zero = n_zero,
      prop_zero = prop_zero,
      spike_ratio = spike_ratio,
      prop_flag = prop_flag,
      spike_flag = spike_flag,
      flagged = isTRUE(prop_flag) || isTRUE(spike_flag)
    )
  })
  names(out) <- candidate_cols
  Filter(Negate(is.null), out)
}
