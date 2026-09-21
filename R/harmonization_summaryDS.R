#' @title Non-disclosive harmonization summary
#' @description Produces exactly the aggregate statistics needed to build a
#'   harmonization report/figures client-side, and nothing else -- no
#'   row-level value ever leaves the server. Every returned number is either
#'   a count, a percentage, or a summary statistic (mean/sd/quantile) with a
#'   minimum-count floor enforced first via \code{nfilter}.
#'
#' @param df A data frame.
#' @param nfilter Minimum number of non-missing observations required before
#'   a numeric summary or a categorical cell count is released. Categorical
#'   cells below this count are reported as \code{NA} (suppressed), same
#'   convention as other DataSHIELD packages (e.g. \code{ds.table}).
#' @param max_categories_shown Distinct-value threshold above which a
#'   categorical column's full label:count breakdown is replaced with
#'   extremes only (no labels) -- see \code{cdh_categorical_summary}. Default 20.
#'
#' @return A list:
#'   \itemize{
#'     \item \code{n}: total row count at this site.
#'     \item \code{missing_pct}: named numeric vector, % missing per column.
#'     \item \code{numeric_summary}: named list, one entry per numeric column
#'       with \code{n, mean, sd, min, q25, median, q75, max} (entry omitted
#'       entirely if non-missing n < nfilter).
#'     \item \code{categorical_summary}: named list, one entry per
#'       non-numeric column -- see \code{cdh_categorical_summary} for the
#'       \code{"full"} vs. \code{"extremes"} shape.
#'   }
#' @export
harmonization_summaryDS <- function(df, nfilter = 5, max_categories_shown = 20) {

  n <- nrow(df)
  if (n < nfilter) stop("site n below disclosure threshold")

  missing_pct <- sapply(df, function(col) sum(is.na(col)) / length(col) * 100)

  numeric_cols <- names(df)[sapply(df, is.numeric)]
  numeric_summary <- lapply(numeric_cols, function(cn) {
    x <- df[[cn]]
    x <- x[!is.na(x)]
    if (length(x) < nfilter) return(NULL)
    list(
      n = length(x),
      mean = mean(x),
      sd = stats::sd(x),
      min = min(x),
      q25 = stats::quantile(x, 0.25, names = FALSE),
      median = stats::median(x),
      q75 = stats::quantile(x, 0.75, names = FALSE),
      max = max(x)
    )
  })
  names(numeric_summary) <- numeric_cols
  numeric_summary <- Filter(Negate(is.null), numeric_summary)

  # ---- categorical frequencies (full breakdown only for bounded category
  # sets; high-cardinality/free-text-like columns get extremes only, no
  # labels -- see cdh_categorical_summary) --------------------------------
  categorical_cols <- setdiff(names(df), numeric_cols)
  categorical_summary <- lapply(categorical_cols, function(cn) {
    cdh_categorical_summary(df[[cn]], nfilter = nfilter, max_categories_shown = max_categories_shown)
  })
  names(categorical_summary) <- categorical_cols

  list(
    n = n,
    missing_pct = missing_pct,
    numeric_summary = numeric_summary,
    categorical_summary = categorical_summary
  )
}
