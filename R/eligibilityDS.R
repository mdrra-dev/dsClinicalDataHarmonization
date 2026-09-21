#' @title Apply the study's agreed eligibility criteria, patient-visit level
#' @description Filters to patients/visits meeting ALL of:
#'   \itemize{
#'     \item Diagnosis from \code{min_year_diagnosis} onwards (\code{Year_diagnosis >= min_year_diagnosis}).
#'     \item Age at diagnosis >= \code{min_age}.
#'     \item At least \code{min_visits_after_cutoff} visit(s) occurring at or
#'       after \code{followup_cutoff_year} (derived per-visit calendar year =
#'       \code{Year_diagnosis + floor(Visit_months_from_diagnosis / 12)} --
#'       this package has no direct visit-calendar-year column, so this is
#'       an explicit, documented derivation, not an exact recorded date).
#'     \item >= \code{min_followup_months_after_cutoff} months of follow-up
#'       SPAN among those at-or-after-cutoff visits (max minus min
#'       \code{Visit_months_from_diagnosis} among them).
#'   }
#'   Cohort (from the \code{pat_ID} prefix -- \code{pat_ID} is
#'   character/alphanumeric, e.g. "SE1") is derived internally and used only
#'   to report eligibility BY COHORT, per the protocol's note that selection
#'   criteria may differ by centre and must be documented per cohort before
#'   pooling -- this function does not change the criteria by cohort, only
#'   the reporting.
#' @param df A data frame (already resolved).
#' @param pat_id_col,visit_col,year_diagnosis_col,age_diagnosis_col Column names.
#' @param min_year_diagnosis,min_age,followup_cutoff_year,
#'   min_followup_months_after_cutoff,min_visits_after_cutoff Criteria thresholds.
#' @param prefix_regex Cohort-code regex, applied to \code{pat_id_col}.
#' @param nfilter Disclosure floor for per-cohort counts.
#' @param newobj Name under which the filtered (eligible-patients-only) data
#'   frame is stored.
#' @return \code{list(newobj=, n_patients_before=, n_patients_eligible=,
#'   n_rows_before=, n_rows_after=, by_cohort=)} -- \code{by_cohort} is a
#'   named list (cohort -> n_patients, n_eligible, proportion), cohorts with
#'   fewer than \code{nfilter} patients omitted.
#' @export
apply_eligibility_criteriaDS <- function(df, pat_id_col = "pat_ID",
                                          visit_col = "Visit_months_from_diagnosis",
                                          year_diagnosis_col = "Year_diagnosis",
                                          age_diagnosis_col = "Age_diagnosis",
                                          min_year_diagnosis = 2006, min_age = 18,
                                          followup_cutoff_year = 2010,
                                          min_followup_months_after_cutoff = 24,
                                          min_visits_after_cutoff = 1,
                                          prefix_regex = "^[A-Za-z]+", nfilter = 5, newobj) {

  for (nm in c(pat_id_col, visit_col, year_diagnosis_col, age_diagnosis_col)) {
    if (!nm %in% names(df)) stop("column '", nm, "' not found in data")
  }

  pid_chr <- as.character(df[[pat_id_col]])
  cohort <- regmatches(pid_chr, regexpr(prefix_regex, pid_chr))
  cohort[cohort == ""] <- NA

  pat_ids <- unique(df[[pat_id_col]])
  n_patients_before <- length(pat_ids)

  elig <- vapply(pat_ids, function(pid) {
    sub <- df[df[[pat_id_col]] == pid, , drop = FALSE]

    year_diag <- sub[[year_diagnosis_col]][!is.na(sub[[year_diagnosis_col]])][1]
    age_diag  <- sub[[age_diagnosis_col]][!is.na(sub[[age_diagnosis_col]])][1]
    year_ok <- !is.na(year_diag) && year_diag >= min_year_diagnosis
    age_ok  <- !is.na(age_diag) && age_diag >= min_age

    visit_calendar_year <- year_diag + floor(sub[[visit_col]] / 12)
    after_cutoff <- !is.na(visit_calendar_year) & visit_calendar_year >= followup_cutoff_year
    n_after <- sum(after_cutoff)
    span_after <- if (n_after > 0) {
      diff(range(sub[[visit_col]][after_cutoff], na.rm = TRUE))
    } else 0

    isTRUE(year_ok) && isTRUE(age_ok) && n_after >= min_visits_after_cutoff &&
      span_after >= min_followup_months_after_cutoff
  }, logical(1))
  names(elig) <- as.character(pat_ids)

  eligible_ids <- pat_ids[elig]
  df_filtered <- df[df[[pat_id_col]] %in% eligible_ids, , drop = FALSE]

  cohort_by_pat <- cohort[match(pat_ids, df[[pat_id_col]])]
  cohorts <- unique(cohort_by_pat[!is.na(cohort_by_pat)])
  by_cohort <- setNames(lapply(cohorts, function(co) {
    idx <- which(cohort_by_pat == co)
    n <- length(idx)
    if (n < nfilter) return(NULL)
    n_elig <- sum(elig[idx])
    list(n_patients = n, n_eligible = n_elig, proportion = n_elig / n)
  }), cohorts)
  by_cohort <- Filter(Negate(is.null), by_cohort)

  base::assign(newobj, df_filtered, envir = parent.frame())
  list(newobj = newobj, n_patients_before = n_patients_before,
       n_patients_eligible = length(eligible_ids),
       n_rows_before = nrow(df), n_rows_after = nrow(df_filtered),
       by_cohort = by_cohort)
}

#' @title Empirical CDF of per-patient follow-up duration (Objective 3 support)
#' @description Per-patient follow-up duration = \code{max(visit_col)} for
#'   that patient (assumes baseline = time 0, consistent with
#'   \code{d2t_followup_statsDS}). Returns a disclosure-safe binned
#'   empirical CDF/survival curve (proportion of patients with duration >=
#'   each grid point), so the analyst can visually/numerically judge how
#'   many patients meet a duration threshold (e.g. the 24-month minimum)
#'   without any per-patient value ever leaving the server. Optionally by group.
#' @param df A data frame (already resolved).
#' @param pat_id_col,visit_col Column names.
#' @param group_col Optional grouping column (e.g. \code{"cohort"}), baseline
#'   (first) value per patient.
#' @param num_points Number of grid points across the observed duration range.
#' @param min_months The threshold to highlight explicitly (default 24, the
#'   protocol's minimum).
#' @param nfilter Disclosure floor per group.
#' @return \code{list(grid=, groups=)} -- \code{groups} is a named list (or a
#'   single \code{"all"} entry if no \code{group_col}), each with
#'   \code{n_patients}, \code{prop_meeting_min} (at \code{min_months}), and
#'   \code{survival_curve} (proportion with duration >= each \code{grid}
#'   point, \code{NA} where the underlying count is below \code{nfilter}).
#' @export
followup_duration_cdfDS <- function(df, pat_id_col = "pat_ID",
                                     visit_col = "Visit_months_from_diagnosis",
                                     group_col = NULL, num_points = 20, min_months = 24,
                                     nfilter = 5) {

  per_pat <- stats::aggregate(df[[visit_col]], by = list(pat = df[[pat_id_col]]), FUN = max, na.rm = TRUE)
  names(per_pat) <- c("pat_id", "followup")

  if (!is.null(group_col) && group_col %in% names(df)) {
    grp_lookup <- df[!duplicated(df[[pat_id_col]]), c(pat_id_col, group_col)]
    per_pat[[group_col]] <- grp_lookup[[group_col]][match(per_pat$pat_id, grp_lookup[[pat_id_col]])]
  } else {
    per_pat[["__all__"]] <- "all"
    group_col <- "__all__"
  }

  rng <- range(per_pat$followup, na.rm = TRUE)
  grid <- seq(rng[1], rng[2], length.out = num_points)
  if (!min_months %in% grid) grid <- sort(c(grid, min_months))

  groups <- unique(per_pat[[group_col]][!is.na(per_pat[[group_col]])])
  out <- setNames(lapply(groups, function(g) {
    x <- per_pat$followup[per_pat[[group_col]] == g]
    n <- length(x)
    if (n < nfilter) return(NULL)
    curve <- vapply(grid, function(thr) {
      cnt <- sum(x >= thr)
      if (cnt > 0 && cnt < nfilter) return(NA_real_)
      cnt / n
    }, numeric(1))
    list(n_patients = n, prop_meeting_min = sum(x >= min_months) / n, survival_curve = curve)
  }), as.character(groups))
  out <- Filter(Negate(is.null), out)

  list(grid = grid, groups = out)
}
