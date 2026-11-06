library(missForest)
#' @title missForest imputation (random-forest, mixed types, keeps all rows)
#' @description Fills missing values with simple placeholders (median for
#'   numerics, mode for categoricals), then iteratively refines them: for each
#'   variable with missing entries, a random forest is trained on the observed
#'   rows using the other variables and used to predict the missing ones.
#'   Iterates until the change in imputed values stops decreasing or
#'   \code{maxiter} is reached.
#'
#'   Unlike \code{longitudinal_imputationDS}, this is a POOLED method: it
#'   learns from all rows at the site, so every row is kept and patients with
#'   a single visit are imputed too. Include the visit column as a predictor
#'   (i.e. do not list it in \code{exclude_cols}) to let it learn time effects.
#'
#' @param df A data frame.
#' @param exclude_cols Character vector (or "$"-joined string) of columns that
#'   must NOT be imputed or used as predictors (e.g. patient ID, free-text
#'   identifiers). They are passed through unchanged.
#' @param maxiter Maximum missForest iterations (capped at 50).
#' @param ntree Trees per forest (capped at 500).
#' @param nfilter Minimum site row count required to proceed at all.
#' @param seed Optional integer for reproducibility.
#'
#' @return \code{list(newobj=, n_rows=, n_cols_imputed=, n_cols_skipped=,
#'   n_missing_before=, n_missing_after=, oob_error=)}.
#' @export
missforest_imputationDS <- function(df, exclude_cols = NULL, maxiter = 10,
                                    ntree = 100, nfilter = 5, seed = NULL,
                                    newobj) {
  suppressWarnings(suppressPackageStartupMessages({
    library(missForest)
  }))

  if (!is.data.frame(df)) stop("df must be a data frame")
  if (nrow(df) < nfilter) stop("site n below disclosure threshold")

  maxiter <- min(as.integer(maxiter), 50L)
  ntree   <- min(as.integer(ntree), 500L)
  if (is.na(maxiter) || maxiter < 1) stop("maxiter must be a positive integer")
  if (is.na(ntree)   || ntree   < 1) stop("ntree must be a positive integer")
  if (!is.null(seed)) set.seed(as.integer(seed))

  exclude <- intersect(cdh_split_cols(exclude_cols), names(df))

  # remember original types so they can be restored after imputation
  orig_class <- lapply(df, function(x) class(x)[1])
  work <- df
  for (nm in setdiff(names(work), exclude)) {
    if (is.character(work[[nm]]) || is.logical(work[[nm]]))
      work[[nm]] <- as.factor(work[[nm]])
  }

  # columns missForest can actually use
  cand <- setdiff(names(work), exclude)
  keep <- vapply(cand, function(nm) {
    x <- work[[nm]]
    if (all(is.na(x))) return(FALSE)                         # nothing to learn from
    if (is.factor(x) && nlevels(x) > 53) return(FALSE)       # randomForest limit
    if (!(is.numeric(x) || is.factor(x))) return(FALSE)      # dates etc.
    TRUE
  }, logical(1))
  use_cols     <- cand[keep]
  skipped_cols <- setdiff(names(work), use_cols)

  n_missing_before <- sum(is.na(work[use_cols]))

  if (length(use_cols) < 2 || n_missing_before == 0) {
    if (length(use_cols) < 2)
      warning("fewer than 2 usable columns; data returned unimputed.")
    base::assign(newobj, as.data.frame(df), envir = parent.frame())
    return(list(newobj = newobj, n_rows = nrow(df), n_cols_imputed = 0L,
                n_cols_skipped = length(skipped_cols),
                n_missing_before = n_missing_before,
                n_missing_after = n_missing_before, oob_error = NA))
  }

  mf <- tryCatch(
    missForest::missForest(as.data.frame(work[use_cols]), maxiter = maxiter,
                           ntree = ntree, verbose = FALSE),
    error = function(e) stop("missForest failed: ", conditionMessage(e))
  )

  out <- df
  for (nm in use_cols) {
    x <- mf$ximp[[nm]]
    cls <- orig_class[[nm]]
    if (cls == "character") x <- as.character(x)
    else if (cls == "logical") x <- as.logical(as.character(x))
    else if (cls == "integer") x <- as.integer(round(x))
    out[[nm]] <- x
  }
  out <- as.data.frame(out)

  base::assign(newobj, out, envir = parent.frame())

  list(newobj = newobj, n_rows = nrow(out), n_cols_imputed = length(use_cols),
       n_cols_skipped = length(skipped_cols),
       n_missing_before = n_missing_before,
       n_missing_after = sum(is.na(out[use_cols])),
       oob_error = as.numeric(mf$OOBerror))
}