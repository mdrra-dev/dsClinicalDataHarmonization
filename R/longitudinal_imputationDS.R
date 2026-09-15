#' @title Longitudinal (patient-grouped) MICE imputation
#' @description The preferred imputation method for this harmonization
#'   module: for each patient, orders their visits chronologically and runs
#'   predictive mean matching (\code{mice}, method = "pmm") WITHIN that
#'   patient's own visit history, then averages across the \code{m} imputed
#'   draws per numeric variable. This mirrors a clinically sensible
#'   assumption -- that a patient's own trajectory is the best predictor of
#'   their missing values -- rather than pooling across unrelated patients.
#'
#'   This is an adapted, corrected version of the originally supplied
#'   \code{impute_missing_data_mice()} sketch:
#'   \itemize{
#'     \item the join key at the end used the literal column \code{Visit}
#'       even when the function was called with a different
#'       \code{visit_col} (e.g. \code{Protocol_visit}) -- fixed to always use
#'       the actual \code{visit_col} argument.
#'     \item \code{quickpred(..., exclude = c("sam_ID","Protocol_visit"))}
#'       hardcoded two specific column names regardless of what the caller's
#'       data actually contained -- replaced with an \code{exclude_cols}
#'       argument the caller controls.
#'     \item a patient with a single visit (or fewer rows than \code{mice}
#'       can meaningfully model) would previously error out the whole call --
#'       such patients are now passed through unimputed with a per-site
#'       warning rather than crashing the pipeline.
#'     \item a disclosure floor (\code{nfilter}) is enforced on site size,
#'       consistent with every other function in this package.
#'   }
#'
#' @param df A data frame containing at least \code{pat_id_col} and
#'   \code{visit_col}.
#' @param pat_id_col Character. Name of the patient identifier column.
#' @param visit_col Character. Name of the visit-ordering column (e.g.
#'   \code{Visit} or \code{Visit_months_from_diagnosis}).
#' @param exclude_cols Character vector (or "$"-joined string), columns to
#'   exclude from \code{mice::quickpred}'s predictor matrix (e.g. free-text
#'   identifiers). \code{pat_id_col} is always excluded automatically.
#' @param m Number of imputations per patient (passed to \code{mice}).
#' @param maxit Maximum MICE iterations per chain.
#' @param nfilter Minimum site row count required to proceed at all.
#' @param newobj Name under which the resulting data frame is stored.
#'
#' @return Disclosure-safe: the imputed data frame (one row per patient,
#'   visit; numeric columns imputed and averaged across the \code{m} draws;
#'   non-numeric columns taken from the modal imputed value) is stored
#'   server-side via \code{base::assign(newobj, ..., envir = parent.frame())} and
#'   never appears in this function's own return value. Returns a list with
#'   \code{newobj}, \code{n_rows}, \code{n_patients}, and
#'   \code{n_patients_unimputed} (patients with <2 visits, left as-is).
#' @export
longitudinal_imputationDS <- function(df, pat_id_col, visit_col,
                                       exclude_cols = NULL, m = 5, maxit = 30,
                                       nfilter = 5,
                                       newobj = "longitudinal_imputation_result") {

  suppressWarnings(suppressPackageStartupMessages({
    library(dplyr)
    library(mice)
    library(purrr)
  }))

  if (nrow(df) < nfilter) stop("site n below disclosure threshold")
  if (!pat_id_col %in% names(df)) stop("pat_id_col '", pat_id_col, "' not found in data")
  if (!visit_col %in% names(df)) stop("visit_col '", visit_col, "' not found in data")

  exclude <- unique(c(pat_id_col, cdh_split_cols(exclude_cols)))
  exclude <- intersect(exclude, names(df))

  numeric_cols <- names(df)[sapply(df, is.numeric)]
  numeric_cols <- setdiff(numeric_cols, c(pat_id_col, visit_col))
  other_cols   <- setdiff(names(df), c(pat_id_col, visit_col, numeric_cols))

  df2 <- df %>%
    dplyr::group_by(.data[[pat_id_col]]) %>%
    dplyr::arrange(.data[[visit_col]], .by_group = TRUE) %>%
    dplyr::ungroup()

  sub_list <- df2 %>%
    dplyr::group_by(.data[[pat_id_col]]) %>%
    dplyr::group_split()

  n_too_small <- sum(vapply(sub_list, nrow, integer(1)) < 2)
  if (n_too_small > 0)
    warning(n_too_small, " patient(s) had fewer than 2 visits and were left ",
            "unimputed (mice needs within-patient variation across visits to fit on).")

  imp_draws_per_patient <- lapply(sub_list, function(sub_df) {
    if (nrow(sub_df) < 2 || !any(is.na(sub_df))) {
      return(list(sub_df))  # nothing to impute, or not enough rows to model
    }
    pm <- tryCatch(
      mice::quickpred(sub_df, exclude = exclude),
      error = function(e) NULL
    )
    imp_obj <- tryCatch(
      mice::mice(sub_df, m = m, method = "pmm", ridge = 1e-5,
                 predictorMatrix = pm, maxit = maxit, printFlag = FALSE),
      error = function(e) {
        warning("mice failed for one patient (", conditionMessage(e),
                "); left unimputed for that patient.")
        NULL
      }
    )
    if (is.null(imp_obj)) return(list(sub_df))
    purrr::map(seq_len(imp_obj$m), ~ mice::complete(imp_obj, .x))
  })

  imp_list_flat <- purrr::flatten(imp_draws_per_patient)
  imp_list_flat <- purrr::map2(
    imp_list_flat, seq_along(imp_list_flat),
    ~ dplyr::mutate(.x, .imputation = .y)
  )
  df_imp_all <- dplyr::bind_rows(imp_list_flat)

  # numeric variables: mean across the m draws per (patient, visit)
  # non-numeric variables: modal (most frequent) value across the m draws
  .mode_chr <- function(x) {
    x <- x[!is.na(x)]
    if (!length(x)) return(NA)
    names(sort(table(x), decreasing = TRUE))[1]
  }

  df_imp <- df_imp_all %>%
    dplyr::group_by(.data[[pat_id_col]], .data[[visit_col]]) %>%
    dplyr::summarise(
      dplyr::across(dplyr::all_of(numeric_cols), ~ mean(.x, na.rm = TRUE)),
      dplyr::across(dplyr::all_of(setdiff(other_cols, ".imputation")), .mode_chr),
      .groups = "drop"
    )

  df_imp <- as.data.frame(df_imp)
  base::assign(newobj, df_imp, envir = parent.frame())

  list(newobj = newobj, n_rows = nrow(df_imp),
       n_patients = length(sub_list), n_patients_unimputed = n_too_small)
}
