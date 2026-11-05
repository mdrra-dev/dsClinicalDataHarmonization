#' @title Longitudinal (patient-grouped) PMM imputation
#' @description For each patient, visits are ordered chronologically and
#'   predictive mean matching (\code{mice}, method = "pmm") is run WITHIN that
#'   patient's own visit history; the \code{m} draws are averaged per cell.
#'
#'   Variable selection:
#'   \itemize{
#'     \item \code{include_cols}: if given, ONLY these columns are imputed and
#'       used as predictors. If NULL, all columns are candidates.
#'     \item \code{exclude_cols}: removed from the candidate set (applied after
#'       \code{include_cols}). \code{pat_id_col} is always removed.
#'     \item \code{visit_col} is never imputed; if numeric, it is used as a
#'       predictor (time effect), otherwise it is only used for ordering.
#'   }
#'
#'   Type guard: only plain numeric columns (integer/double) are imputed.
#'   Character, factor, logical, Date/POSIXt and difftime columns pass through
#'   unchanged. All-NA columns are skipped. Non-finite values (Inf/NaN) are
#'   treated as missing. Only cells that were NA are modified, so every row,
#'   column, column order and column type of the input is preserved.
#'
#' @param df A data frame.
#' @param pat_id_col Patient identifier column.
#' @param visit_col Visit-ordering column.
#' @param include_cols Character vector (or "$"-joined string). Columns to
#'   impute. NULL = all candidates.
#' @param exclude_cols Character vector (or "$"-joined string). Columns to leave out.
#' @param m Imputations per patient (capped at 20).
#' @param maxit MICE iterations (capped at 50).
#' @param min_visits Patients with fewer visits are left unimputed.
#' @param nfilter Minimum site row count required to proceed at all.
#'
#' @return \code{list(newobj, n_rows, n_patients, n_patients_unimputed,
#'   n_patients_failed, n_cols_imputed, n_cols_skipped_type,
#'   n_cols_skipped_allna, n_missing_before, n_missing_after)}.
#' @export
longitudinal_imputationDS <- function(df, pat_id_col, visit_col,
                                      include_cols = NULL, exclude_cols = NULL,
                                      m = 5, maxit = 30, min_visits = 3,
                                      nfilter = 5, newobj) {
  if (!requireNamespace("mice", quietly = TRUE))
    stop("package 'mice' is not installed on this server")

    .chk_chr <- function(x, nm) if (!is.null(x) && !is.character(x))
  stop("'", nm, "' must be a character string or NULL; got ", class(x)[1],
       ". Client and server package versions probably do not match.")
.chk_chr(include_cols, "include_cols")
.chk_chr(exclude_cols, "exclude_cols")
.chk_chr(pat_id_col, "pat_id_col")
.chk_chr(visit_col, "visit_col")
if (!is.numeric(m) || !is.numeric(maxit) || !is.numeric(min_visits) || !is.numeric(nfilter))
  stop("m, maxit, min_visits and nfilter must be numeric; client and server versions probably do not match.")

  #if (!is.data.frame(df)) stop("df must be a data frame")
  if (nrow(df) < nfilter) stop("site n below disclosure threshold")
  if (!pat_id_col %in% names(df)) stop("pat_id_col '", pat_id_col, "' not found in data")
  if (!visit_col  %in% names(df)) stop("visit_col '", visit_col, "' not found in data")

  m          <- min(as.integer(m), 20L)
  maxit      <- min(as.integer(maxit), 50L)
  min_visits <- max(as.integer(min_visits), 2L)
  if (is.na(m) || m < 1)         stop("m must be a positive integer")
  if (is.na(maxit) || maxit < 1) stop("maxit must be a positive integer")

  # ---- 1. candidate columns: include -> minus exclude/id/visit ----------------
  inc <- if (is.null(include_cols)) character(0) else cdh_split_cols(include_cols)
  exc <- if (is.null(exclude_cols)) character(0) else cdh_split_cols(exclude_cols)

  if (length(inc)) {
    not_found <- setdiff(inc, names(df))
    if (length(not_found))
      stop("include_cols not found in data: ", paste(not_found, collapse = ", "))
    cand <- inc
  } else {
    cand <- names(df)
  }
  cand <- setdiff(cand, c(exc, pat_id_col, visit_col))

  # ---- 2. type guard ----------------------------------------------------------
  .is_plain_num <- function(x)
    is.numeric(x) && !inherits(x, c("Date", "POSIXt", "difftime"))

  is_num       <- vapply(df[cand], .is_plain_num, logical(1))
  skipped_type <- cand[!is_num]
  num_cols     <- cand[is_num]

  # work on a copy; non-finite values count as missing
  work <- df
  for (nm in num_cols) {
    x <- work[[nm]]
    x[!is.finite(x)] <- NA
    work[[nm]] <- x
  }

  all_na       <- vapply(work[num_cols], function(x) all(is.na(x)), logical(1))
  skipped_allna <- num_cols[all_na]
  num_cols     <- num_cols[!all_na]

  n_missing_before <- if (length(num_cols)) sum(is.na(work[num_cols])) else 0L

  # ---- 3. order rows by patient then visit ------------------------------------
  ord  <- order(df[[pat_id_col]], df[[visit_col]], na.last = TRUE)
  df2  <- df[ord, , drop = FALSE]
  work <- work[ord, , drop = FALSE]
  out  <- df2

  grp <- as.character(df2[[pat_id_col]])
  grp[is.na(grp)] <- "<NA>"
  idx_list <- split(seq_len(nrow(df2)), factor(grp, levels = unique(grp)))

  # visit column as predictor only if it is plain numeric
  visit_pred <- .is_plain_num(work[[visit_col]])
  mice_cols  <- c(num_cols, if (visit_pred) visit_col)

  n_small <- 0L
  n_failed <- 0L

  if (length(num_cols) > 0 && n_missing_before > 0) {
    for (ix in idx_list) {
      sub <- work[ix, mice_cols, drop = FALSE]
      if (!anyNA(sub[num_cols])) next
      if (length(ix) < min_visits) { n_small <- n_small + 1L; next }

      draws <- tryCatch({
        pm <- tryCatch(suppressWarnings(mice::quickpred(sub)),
                       error = function(e) mice::make.predictorMatrix(sub))
        meth <- ifelse(names(sub) == visit_col, "", "pmm")
        imp <- suppressWarnings(
          mice::mice(sub, m = m, method = meth, ridge = 1e-5,
                     predictorMatrix = pm, maxit = maxit, printFlag = FALSE)
        )
        lapply(seq_len(imp$m), function(k) mice::complete(imp, k))
      }, error = function(e) NULL)

      if (is.null(draws)) { n_failed <- n_failed + 1L; next }

      for (nm in num_cols) {
        cells <- is.na(work[[nm]][ix])
        if (!any(cells)) next
        mat <- vapply(draws, function(d) as.numeric(d[[nm]]), numeric(length(ix)))
        if (is.null(dim(mat))) mat <- matrix(mat, nrow = length(ix))
        val <- rowMeans(mat)
        if (is.integer(df2[[nm]])) val <- as.integer(round(val))
        out[[nm]][ix[cells]] <- val[cells]
      }
    }
  }

  out <- as.data.frame(out)
  rownames(out) <- NULL
  base::assign(newobj, out, envir = parent.frame())

  list(newobj = newobj, n_rows = nrow(out), n_patients = length(idx_list),
       n_patients_unimputed = n_small, n_patients_failed = n_failed,
       n_cols_imputed = length(num_cols),
       n_cols_skipped_type = length(skipped_type),
       n_cols_skipped_allna = length(skipped_allna),
       n_missing_before = n_missing_before,
       n_missing_after = if (length(num_cols)) sum(is.na(out[num_cols])) else 0L)
}

# #' @title Longitudinal (patient-grouped) MICE imputation
# #' @description The preferred imputation method for this harmonization
# #'   module: for each patient, orders their visits chronologically and runs
# #'   predictive mean matching (\code{mice}, method = "pmm") WITHIN that
# #'   patient's own visit history, then averages across the \code{m} imputed
# #'   draws per numeric variable. This mirrors a clinically sensible
# #'   assumption -- that a patient's own trajectory is the best predictor of
# #'   their missing values -- rather than pooling across unrelated patients.
# #'
# #'   This is an adapted, corrected version of the originally supplied
# #'   \code{impute_missing_data_mice()} sketch:
# #'   \itemize{
# #'     \item the join key at the end used the literal column \code{Visit}
# #'       even when the function was called with a different
# #'       \code{visit_col} (e.g. \code{Protocol_visit}) -- fixed to always use
# #'       the actual \code{visit_col} argument.
# #'     \item \code{quickpred(..., exclude = c("sam_ID","Protocol_visit"))}
# #'       hardcoded two specific column names regardless of what the caller's
# #'       data actually contained -- replaced with an \code{exclude_cols}
# #'       argument the caller controls.
# #'     \item a patient with a single visit (or fewer rows than \code{mice}
# #'       can meaningfully model) would previously error out the whole call --
# #'       such patients are now passed through unimputed with a per-site
# #'       warning rather than crashing the pipeline.
# #'     \item a disclosure floor (\code{nfilter}) is enforced on site size,
# #'       consistent with every other function in this package.
# #'   }
# #'
# #' @param df A data frame containing at least \code{pat_id_col} and
# #'   \code{visit_col}.
# #' @param pat_id_col Character. Name of the patient identifier column.
# #' @param visit_col Character. Name of the visit-ordering column (e.g.
# #'   \code{Visit} or \code{Visit_months_from_diagnosis}).
# #' @param exclude_cols Character vector (or "$"-joined string), columns to
# #'   exclude from \code{mice::quickpred}'s predictor matrix (e.g. free-text
# #'   identifiers). \code{pat_id_col} is always excluded automatically.
# #' @param m Number of imputations per patient (passed to \code{mice}).
# #' @param maxit Maximum MICE iterations per chain.
# #' @param nfilter Minimum site row count required to proceed at all.
# #'
# #' @return \code{list(newobj=, n_rows=, n_patients=, n_patients_unimputed=)}.
# #' @export
# longitudinal_imputationDS <- function(df, pat_id_col, visit_col,
#                                        exclude_cols = NULL, m = 5, maxit = 30,
#                                        nfilter = 5, newobj) {
#   suppressWarnings(suppressPackageStartupMessages({
#     library(dplyr)
#     library(mice)
#     library(purrr)
#   }))

#   if (nrow(df) < nfilter) stop("site n below disclosure threshold")
#   if (!pat_id_col %in% names(df)) stop("pat_id_col '", pat_id_col, "' not found in data")
#   if (!visit_col %in% names(df)) stop("visit_col '", visit_col, "' not found in data")

#   exclude <- unique(c(pat_id_col, cdh_split_cols(exclude_cols)))
#   exclude <- intersect(exclude, names(df))

#   numeric_cols <- names(df)[sapply(df, is.numeric)]
#   numeric_cols <- setdiff(numeric_cols, c(pat_id_col, visit_col))
#   other_cols   <- setdiff(names(df), c(pat_id_col, visit_col, numeric_cols))

#   df2 <- df %>%
#     dplyr::group_by(.data[[pat_id_col]]) %>%
#     dplyr::arrange(.data[[visit_col]], .by_group = TRUE) %>%
#     dplyr::ungroup()

#   sub_list <- df2 %>%
#     dplyr::group_by(.data[[pat_id_col]]) %>%
#     dplyr::group_split()

#   n_too_small <- sum(vapply(sub_list, nrow, integer(1)) < 2)
#   if (n_too_small > 0)
#     warning(n_too_small, " patient(s) had fewer than 2 visits and were left ",
#             "unimputed (mice needs within-patient variation across visits to fit on).")

#   imp_draws_per_patient <- lapply(sub_list, function(sub_df) {
#     if (nrow(sub_df) < 2 || !any(is.na(sub_df))) {
#       return(list(sub_df))  # nothing to impute, or not enough rows to model
#     }
#     pm <- tryCatch(
#       mice::quickpred(sub_df, exclude = exclude),
#       error = function(e) NULL
#     )

#     # 1) replace purrr::flatten
# imp_list_flat <- unlist(imp_draws_per_patient, recursive = FALSE)

# # 2) put complete() inside the tryCatch
# imp_obj_draws <- tryCatch(
#   {
#     imp_obj <- mice::mice(sub_df, m = m, method = "pmm", ridge = 1e-5,
#                           predictorMatrix = pm, maxit = maxit, printFlag = FALSE)
#     if (is.null(imp_obj)) return(list(sub_df))
#     purrr::map(seq_len(imp_obj$m), ~ mice::complete(imp_obj, .x))
#   },
#   error = function(e) {
#     warning("mice failed for one patient (", conditionMessage(e), "); left unimputed.")
#     # 3) check packages
#     for (p in c("dplyr", "mice", "purrr"))
#       if (!requireNamespace(p, quietly = TRUE)) stop("package '", p, "' not installed on this server")

#     NULL
#   }
# )
# if (is.null(imp_obj_draws)) return(list(sub_df))
# imp_obj_draws

#   #   imp_obj <- tryCatch(
#   #     mice::mice(sub_df, m = m, method = "pmm", ridge = 1e-5,
#   #                predictorMatrix = pm, maxit = maxit, printFlag = FALSE),
#   #     error = function(e) {
#   #       warning("mice failed for one patient (", conditionMessage(e),
#   #               "); left unimputed for that patient.")
#   #       NULL
#   #     }
#   #   )
#   #   if (is.null(imp_obj)) return(list(sub_df))
#   #   purrr::map(seq_len(imp_obj$m), ~ mice::complete(imp_obj, .x))
#   # 

#   })



#   imp_list_flat <- purrr::flatten(imp_draws_per_patient)
#   imp_list_flat <- purrr::map2(
#     imp_list_flat, seq_along(imp_list_flat),
#     ~ dplyr::mutate(.x, .imputation = .y)
#   )
#   df_imp_all <- dplyr::bind_rows(imp_list_flat)

#   # numeric variables: mean across the m draws per (patient, visit)
#   # non-numeric variables: modal (most frequent) value across the m draws
#   .mode_chr <- function(x) {
#     x <- x[!is.na(x)]
#     if (!length(x)) return(NA)
#     names(sort(table(x), decreasing = TRUE))[1]
#   }

#   df_imp <- df_imp_all %>%
#     dplyr::group_by(.data[[pat_id_col]], .data[[visit_col]]) %>%
#     dplyr::summarise(
#       dplyr::across(dplyr::all_of(numeric_cols), ~ mean(.x, na.rm = TRUE)),
#       dplyr::across(dplyr::all_of(setdiff(other_cols, ".imputation")), .mode_chr),
#       .groups = "drop"
#     )

#   df_imp <- as.data.frame(df_imp)

#   base::assign(newobj, df_imp, envir = parent.frame())

#   list(newobj = newobj, n_rows = nrow(df_imp), n_patients = length(sub_list),
#        n_patients_unimputed = n_too_small)
# }
