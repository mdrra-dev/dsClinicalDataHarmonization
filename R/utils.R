#' @title Parse a column-list argument consistently
#' @description Internal helper used by every function in this package that
#'   accepts a list of column names. Historically some functions expected a
#'   single \code{"$"}-joined string (\code{strsplit(x, "$", fixed=TRUE)[[1]]})
#'   while their client-side wrappers sometimes passed a raw character vector
#'   instead (e.g. the old \code{ds.cast_to_factor()} built a joined string
#'   but then called \code{cast_to_factorDS()} with the raw vector anyway).
#'   Passing a raw vector of length > 1 into the old \code{strsplit(...)[[1]]}
#'   pattern silently returned ONLY the split of the first element, dropping
#'   every other requested column with no warning.
#'
#'   This helper accepts either form so both the "$"-joined-string convention
#'   and a plain character vector work identically everywhere in the package,
#'   and every DS function below now goes through this single implementation
#'   instead of repeating (and risking re-diverging) the parsing logic.
#'
#' @param x \code{NULL}, a character vector, or a single \code{"$"}-joined string.
#' @return A character vector (possibly \code{character(0)} if \code{x} is
#'   \code{NULL} or empty).
#' @export
cdh_split_cols <- function(x) {
  if (is.null(x)) return(character(0))
  if (length(x) > 1) return(x)
  if (!nzchar(x)) return(character(0))
  strsplit(x, "$", fixed = TRUE)[[1]]
}

#' @title Summarize one categorical column without leaking category labels
#'   for high-cardinality columns
#' @description Shared by \code{harmonization_summaryDS} and
#'   \code{exploratory_analysisDS}. A categorical column with a small,
#'   bounded set of known values (e.g. \code{Sex}, \code{csDMARD1}) is safe
#'   to summarize as a full label -> count table, nfilter-suppressed cell by
#'   cell -- this is standard, accepted DataSHIELD practice (the same thing
#'   \code{ds.table1D} does). But an open-ended or free-text column (e.g.
#'   \code{Comorbidities}) can have as many distinct "categories" as there
#'   are patients, at which point a full breakdown is equivalent to
#'   returning row-level data one label at a time. This function draws that
#'   line at \code{max_categories_shown} distinct values: at or below it,
#'   the full nfilter-suppressed table is returned; above it, ONLY the
#'   extremes (largest and smallest surviving counts) are returned, with NO
#'   category label attached to either number.
#'
#' @param x A vector (the column).
#' @param nfilter Minimum count required before a cell/extreme is released.
#' @param max_categories_shown Distinct-value threshold above which only
#'   extremes (no labels) are returned. Default 20.
#' @return A list with \code{type} (\code{"full"} or \code{"extremes"}) and
#'   either \code{counts} (named list, \code{type == "full"}) or
#'   \code{n_distinct}, \code{max_count}, \code{min_count}
#'   (\code{type == "extremes"} -- \code{NA} if no category clears \code{nfilter}).
#' @export
cdh_categorical_summary <- function(x, nfilter = 5, max_categories_shown = 20) {
  tab <- table(x, useNA = "no")
  n_distinct <- length(tab)

  if (n_distinct <= max_categories_shown) {
    tab[tab < nfilter] <- NA
    return(list(type = "full", counts = as.list(tab)))
  }

  surviving <- tab[tab >= nfilter]
  list(
    type = "extremes",
    n_distinct = n_distinct,
    max_count = if (length(surviving)) max(surviving) else NA_integer_,
    min_count = if (length(surviving)) min(surviving) else NA_integer_
  )
}

#' @title Pairwise-complete Spearman correlation matrix, nfilter-suppressed
#' @description Shared by \code{exploratory_analysisDS} and the label
#'   analysis module. Cells whose pairwise-complete observation count is
#'   below \code{nfilter} are set to \code{NA} rather than released.
#' @param df A data frame.
#' @param cols Character vector of numeric column names to include.
#' @param nfilter Minimum pairwise-complete count required to release a cell.
#' @param method Correlation method, default \code{"spearman"} (ordinal/
#'   non-normal clinical scores are common in this package's use case).
#' @return \code{list(cols=, matrix=)}, or \code{NULL} if fewer than 2 columns.
#' @export
cdh_correlation_matrix <- function(df, cols, nfilter = 5, method = "spearman") {
  cols <- intersect(cols, names(df))
  cols <- cols[vapply(df[cols], is.numeric, logical(1))]
  if (length(cols) < 2) return(NULL)

  mat <- as.matrix(df[cols])
  cor_mat <- suppressWarnings(stats::cor(mat, use = "pairwise.complete.obs", method = method))
  pair_n <- crossprod(!is.na(mat))
  cor_mat[pair_n < nfilter] <- NA
  list(cols = cols, matrix = cor_mat)
}

#' @title Derive a "before"/"after" period factor from a visit column
#' @description Shared by every longitudinal (before/after visit) function in
#'   the label analysis module, so the period boundary is applied identically
#'   everywhere: \code{visit_col < visit_cutoff} is \code{"before"},
#'   \code{visit_col >= visit_cutoff} is \code{"after"}. Rows with \code{NA}
#'   in \code{visit_col} get \code{NA} period (excluded downstream by the
#'   usual complete-case handling).
#' @export
cdh_period_factor <- function(df, visit_col, visit_cutoff) {
  factor(ifelse(is.na(df[[visit_col]]), NA,
                ifelse(df[[visit_col]] < visit_cutoff, "before", "after")),
         levels = c("before", "after"))
}
