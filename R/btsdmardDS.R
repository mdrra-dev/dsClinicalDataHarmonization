#' @title Derive btsDMARD from bDMARD and tsDMARD (before imputation)
#' @description Combines the two biologic/targeted-synthetic treatment
#'   columns into one, per the coding scheme: \code{bDMARD} values kept as-is
#'   (anti-TNF=1, anti-IL6=2, rituximab=3, abatacept=4, anti-IL1=5);
#'   \code{tsDMARD} recoded onto the same scale (tofa=1->6, bari=2->7,
#'   upa=3->8, filgo=4->9). For each row: \code{btsDMARD} = the (recoded)
#'   value of whichever of \code{bDMARD}/\code{tsDMARD} is non-missing; if
#'   BOTH are missing, \code{btsDMARD = 0} (explicit imputation, done here
#'   -- BEFORE the general imputation step -- so the general imputer never
#'   has to guess a treatment class). If both are non-missing and disagree,
#'   \code{bDMARD} wins (a patient shouldn't be on both classes
#'   simultaneously under this coding; disagreement is reported, not
#'   silently resolved).
#'
#'   Run \code{ds.check_btsdmard_presence()} first if you just want the
#'   "is at least one of bDMARD/tsDMARD present" check without creating a
#'   new object.
#'
#' @param df A data frame (already resolved).
#' @param bDMARD_col,tsDMARD_col Column names. Defaults \code{"bDMARD"}/\code{"tsDMARD"}.
#' @param newobj Name under which the result is stored.
#' @return \code{list(newobj=, n_rows=, n_from_bDMARD=, n_from_tsDMARD=,
#'   n_imputed_zero=, n_both_present_disagreeing=)}.
#' @export
derive_btsdmardDS <- function(df, bDMARD_col = "bDMARD", tsDMARD_col = "tsDMARD", newobj) {
  if (!bDMARD_col %in% names(df)) stop("bDMARD_col '", bDMARD_col, "' not found in data")
  if (!tsDMARD_col %in% names(df)) stop("tsDMARD_col '", tsDMARD_col, "' not found in data")

  b <- suppressWarnings(as.integer(as.character(df[[bDMARD_col]])))
  ts_recode <- c(`1` = 6, `2` = 7, `3` = 8, `4` = 9)
  ts_raw <- suppressWarnings(as.integer(as.character(df[[tsDMARD_col]])))
  ts <- unname(ts_recode[as.character(ts_raw)])

  b_present <- !is.na(b); ts_present <- !is.na(ts)
  both_present_disagreeing <- b_present & ts_present  # can't both be non-missing under this scheme

  btsDMARD <- ifelse(b_present, b, ifelse(ts_present, ts, 0L))
  df$btsDMARD <- as.integer(btsDMARD)

  base::assign(newobj, df, envir = parent.frame())
  list(newobj = newobj, n_rows = nrow(df),
       n_from_bDMARD = sum(b_present), n_from_tsDMARD = sum(ts_present & !b_present),
       n_imputed_zero = sum(!b_present & !ts_present),
       n_both_present_disagreeing = sum(both_present_disagreeing))
}

#' @title Check whether at least one of bDMARD/tsDMARD is present, per row
#' @description Non-disclosive counts only -- read-only, no object created.
#' @param df A data frame (already resolved).
#' @param bDMARD_col,tsDMARD_col Column names.
#' @param nfilter Disclosure floor.
#' @return \code{list(n_rows=, n_either_present=, n_neither_present=,
#'   prop_either_present=)}.
#' @export
check_btsdmard_presenceDS <- function(df, bDMARD_col = "bDMARD", tsDMARD_col = "tsDMARD", nfilter = 5) {
  if (nrow(df) < nfilter) stop("site n below disclosure threshold")
  b_present <- !is.na(df[[bDMARD_col]])
  ts_present <- !is.na(df[[tsDMARD_col]])
  either <- b_present | ts_present
  list(n_rows = nrow(df), n_either_present = sum(either), n_neither_present = sum(!either),
       prop_either_present = mean(either))
}
