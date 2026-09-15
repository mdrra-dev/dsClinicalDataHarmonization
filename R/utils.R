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
#' @keywords internal
cdh_split_cols <- function(x) {
  if (is.null(x)) return(character(0))
  if (length(x) > 1) return(x)
  if (!nzchar(x)) return(character(0))
  strsplit(x, "$", fixed = TRUE)[[1]]
}
