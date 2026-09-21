#' @title Non-disclosive exploratory analysis
#' @description Computes descriptive statistics and plotting-ready summaries
#'   for exploratory data analysis, without ever returning a row-level value.
#'   Companion to \code{harmonization_summaryDS} (which covers missingness-
#'   oriented reporting); this one is for understanding the SHAPE of the
#'   (typically post-cleaning) data: distributions, category frequencies,
#'   correlations between numeric variables, and simple group-wise trends
#'   (e.g. a variable's mean at each visit number).
#'
#'   Every returned number is either a count/proportion, a summary statistic
#'   with an \code{nfilter} floor, or a bin/cell count with an \code{nfilter}
#'   floor -- the same disclosure-control convention used throughout this
#'   package. Numeric distributions are summarized as BINNED HISTOGRAM
#'   COUNTS (shared bin edges built from the column's own min/max), never as
#'   raw values, mirroring the federated-histogram approach used elsewhere
#'   in this codebase for exactly the same reason: a bin count summarizes an
#'   interval, not one identifiable value.
#'
#' @param df A data frame.
#' @param numeric_cols,categorical_cols Character vectors (or "$"-joined
#'   strings) restricting which columns to summarize. \code{NULL} (default):
#'   every numeric / every non-numeric column respectively.
#' @param group_col Optional column (e.g. \code{"Visit"}) to compute simple
#'   group-wise means of each numeric column against -- useful for a
#'   longitudinal trend plot. \code{NULL} (default): skipped.
#' @param num_bins Number of bins for the numeric histograms. Default 20.
#' @param nfilter Minimum count required before any statistic/cell/bin is released.
#' @param max_categories_shown Distinct-value threshold above which a
#'   categorical column's full label:count breakdown is replaced with
#'   extremes only (no labels) -- see \code{cdh_categorical_summary}. Default 20.
#'
#' @return A list:
#'   \itemize{
#'     \item \code{n}: row count at this site.
#'     \item \code{numeric_summary}: as in \code{harmonization_summaryDS}
#'       (n, mean, sd, min, q25, median, q75, max per numeric column).
#'     \item \code{numeric_histograms}: named list, one entry per numeric
#'       column with \code{breaks} (bin edges) and \code{counts} (per-bin
#'       counts, \code{NA} where a bin's count is below \code{nfilter}).
#'     \item \code{categorical_summary}: named list, one entry per
#'       categorical column -- see \code{cdh_categorical_summary} for the
#'       \code{"full"} (small, bounded category sets) vs. \code{"extremes"}
#'       (high-cardinality/free-text-like columns, no labels ever returned)
#'       shape.
#'     \item \code{correlation}: \code{list(cols=, matrix=)}, a Pearson
#'       correlation matrix over numeric columns with pairwise-complete
#'       observations, cells set to \code{NA} where the pairwise complete
#'       count is below \code{nfilter}. \code{NULL} if fewer than 2 numeric
#'       columns are available.
#'     \item \code{group_trend}: \code{list(levels=, means=)}, per-numeric-
#'       column mean at each level of \code{group_col} (\code{NA} where the
#'       group's count is below \code{nfilter}). \code{NULL} if
#'       \code{group_col} wasn't supplied or isn't present.
#'   }
#' @export
exploratory_analysisDS <- function(df, numeric_cols = NULL, categorical_cols = NULL,
                                    group_col = NULL, num_bins = 20, nfilter = 5,
                                    max_categories_shown = 20) {

  n <- nrow(df)
  if (n < nfilter) stop("site n below disclosure threshold")

  all_numeric <- names(df)[sapply(df, is.numeric)]
  numeric_cols <- if (is.null(numeric_cols)) all_numeric else intersect(cdh_split_cols(numeric_cols), names(df))
  categorical_cols <- if (is.null(categorical_cols)) setdiff(names(df), all_numeric) else intersect(cdh_split_cols(categorical_cols), names(df))

  # ---- numeric summary -----------------------------------------------------
  numeric_summary <- lapply(numeric_cols, function(cn) {
    x <- df[[cn]]; x <- x[!is.na(x)]
    if (length(x) < nfilter) return(NULL)
    list(n = length(x), mean = mean(x), sd = stats::sd(x), min = min(x),
         q25 = stats::quantile(x, 0.25, names = FALSE), median = stats::median(x),
         q75 = stats::quantile(x, 0.75, names = FALSE), max = max(x))
  })
  names(numeric_summary) <- numeric_cols
  numeric_summary <- Filter(Negate(is.null), numeric_summary)

  # ---- numeric histograms (binned counts only) -----------------------------
  numeric_histograms <- lapply(names(numeric_summary), function(cn) {
    x <- df[[cn]]; x <- x[!is.na(x)]
    rng <- range(x)
    if (diff(rng) == 0) return(list(breaks = c(rng[1], rng[1] + 1), counts = length(x)))
    breaks <- seq(rng[1], rng[2], length.out = num_bins + 1)
    counts <- as.integer(table(cut(x, breaks = breaks, include.lowest = TRUE)))
    counts[counts > 0 & counts < nfilter] <- NA
    list(breaks = breaks, counts = counts)
  })
  names(numeric_histograms) <- names(numeric_summary)

  # ---- categorical frequencies (full breakdown only for bounded category
  # sets; high-cardinality/free-text-like columns get extremes only, no
  # labels -- see cdh_categorical_summary) --------------------------------
  categorical_summary <- lapply(categorical_cols, function(cn) {
    cdh_categorical_summary(df[[cn]], nfilter = nfilter, max_categories_shown = max_categories_shown)
  })
  names(categorical_summary) <- categorical_cols

  # ---- correlation matrix ---------------------------------------------------
  correlation <- NULL
  if (length(numeric_cols) >= 2) {
    mat <- as.matrix(df[numeric_cols])
    cor_mat <- suppressWarnings(stats::cor(mat, use = "pairwise.complete.obs"))
    pair_n <- crossprod(!is.na(mat))
    cor_mat[pair_n < nfilter] <- NA
    correlation <- list(cols = numeric_cols, matrix = cor_mat)
  }

  # ---- group-wise trend -----------------------------------------------------
  group_trend <- NULL
  if (!is.null(group_col) && group_col %in% names(df)) {
    grp <- df[[group_col]]
    levels_grp <- sort(unique(grp[!is.na(grp)]))
    means <- lapply(numeric_cols, function(cn) {
      vapply(levels_grp, function(lv) {
        x <- df[[cn]][grp == lv]; x <- x[!is.na(x)]
        if (length(x) < nfilter) return(NA_real_)
        mean(x)
      }, numeric(1))
    })
    names(means) <- numeric_cols
    group_trend <- list(levels = levels_grp, means = means)
  }

  list(n = n, numeric_summary = numeric_summary, numeric_histograms = numeric_histograms,
       categorical_summary = categorical_summary, correlation = correlation,
       group_trend = group_trend)
}
