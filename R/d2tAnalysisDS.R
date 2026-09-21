#' @title d2tAnalysisDS.R -- D2T study-protocol objectives 1-5
#' @description Objective 6 (KM/Cox with left-truncation) is deliberately NOT
#'   implemented here -- see run_D2T_exploratory_orchestrator.R for why.
#'   Objective 7 (confounders) reuses multivariable_modelDS/interaction_modelDS
#'   from the label-analysis module; no new server code needed for it.
#' ---------------------------------------------------------------------------

#' @title Derive a cohort/centre column from the pat_ID prefix
#' @description \code{pat_ID} is character/alphanumeric (e.g. "SE1"); the
#'   cohort code is its leading-letters prefix (e.g. "SE"). Stores result
#'   under \code{newobj} via \code{base::assign()}.
#' @param df A data frame (already resolved).
#' @param pat_id_col Patient identifier column. Default \code{"pat_ID"}.
#' @param prefix_regex Regex for the cohort-code prefix. Default
#'   \code{"^[A-Za-z]+"} (leading letters, e.g. "SE" from "SE1").
#' @param newobj Name under which the result is stored.
#' @return \code{list(newobj=, cohorts_found=)} -- cohort CODES (not patient
#'   IDs), a small fixed set, not row-level data.
#' @export
add_cohort_columnDS <- function(df, pat_id_col = "pat_ID", prefix_regex = "^[A-Za-z]+", newobj) {
  if (!pat_id_col %in% names(df)) stop("pat_id_col", pat_id_col, "not found in data")
  pid_chr <- as.character(df[[pat_id_col]])
  df$cohort <- regmatches(pid_chr, regexpr(prefix_regex, pid_chr))
  df$cohort[df$cohort == ""] <- NA
  base::assign(newobj, df, envir = parent.frame())
  list(newobj = newobj, cohorts_found = sort(unique(df$cohort)))
}

#' @title Descriptive table (numeric + categorical) by group, optionally at a
#'   filtered visit selection (Objective 1: baseline / D2T-visit tables)
#' @description Vectorized version of \code{group_summary_statsDS} (numeric)
#'   / \code{cdh_categorical_summary} (categorical) for several variables at
#'   once, optionally restricted to specific rows first:
#'   \itemize{
#'     \item \code{filter_col == filter_value} (e.g. \code{D2T == 1}, for the
#'       "visit of D2T onset" table), and/or
#'     \item first row per patient by \code{visit_col} (the cohort-entry
#'       visit) if \code{first_only = TRUE}.
#'   }
#' @param df A data frame.
#' @param numeric_vars,categorical_vars Character vectors (or "$"-joined) of
#'   column names.
#' @param group_col Grouping column (e.g. \code{"cohort"}).
#' @param filter_col,filter_value Optional row filter applied first.
#' @param first_only Logical; keep only each patient's first row (by
#'   \code{visit_col}) after filtering. Requires \code{pat_id_col}/\code{visit_col}.
#' @param pat_id_col,visit_col Needed only if \code{first_only = TRUE}.
#' @param nfilter Minimum per-group n required to release a group's stats.
#' @param max_categories_shown See \code{cdh_categorical_summary}.
#' @export
d2t_baseline_tableDS <- function(df, numeric_vars = NULL, categorical_vars = NULL,
                                  group_col = "cohort", filter_col = NULL, filter_value = NULL,
                                  first_only = FALSE, pat_id_col = "pat_ID", visit_col = "Visit",
                                  nfilter = 5, max_categories_shown = 20) {

  if (!is.null(filter_col)) {
    if (!filter_col %in% names(df)) stop("filter_col '", filter_col, "' not found in data")
    df <- df[!is.na(df[[filter_col]]) & df[[filter_col]] == filter_value, , drop = FALSE]
  }
  if (isTRUE(first_only)) {
    df <- df[order(df[[pat_id_col]], df[[visit_col]]), , drop = FALSE]
    df <- df[!duplicated(df[[pat_id_col]]), , drop = FALSE]
  }

  numeric_vars <- intersect(cdh_split_cols(numeric_vars), names(df))
  categorical_vars <- intersect(cdh_split_cols(categorical_vars), names(df))

  numeric_summary <- setNames(
    lapply(numeric_vars, function(v) group_summary_statsDS(df, value_col = v, group_col = group_col, nfilter = nfilter)),
    numeric_vars
  )
  categorical_summary <- setNames(
    lapply(categorical_vars, function(v) {
      groups <- unique(df[[group_col]][!is.na(df[[group_col]])])
      setNames(lapply(groups, function(g) {
        x <- df[[v]][df[[group_col]] == g & !is.na(df[[group_col]])]
        if (sum(!is.na(x)) < nfilter) return(NULL)
        cdh_categorical_summary(x, nfilter = nfilter, max_categories_shown = max_categories_shown)
      }), as.character(groups))
    }),
    categorical_vars
  )
  categorical_summary <- lapply(categorical_summary, function(g) Filter(Negate(is.null), g))

  list(n_rows = nrow(df), numeric_summary = numeric_summary, categorical_summary = categorical_summary)
}

#' @title Chi-square association test of several categorical variables with a group column
#' @description Categorical analog of \code{kruskal_by_groupDS}, for
#'   Objective 2's between-cohort heterogeneity check. A variable is skipped
#'   (\code{NA}) if any contingency-table cell used is below \code{nfilter}.
#' @export
chisq_by_groupDS <- function(df, vars, group_col, nfilter = 5) {

  if (!group_col %in% names(df)) stop("group_col '", group_col, "' not found in data")
  vars <- intersect(cdh_split_cols(vars), names(df))

  out <- lapply(vars, function(v) {
    pair <- stats::complete.cases(df[[v]], df[[group_col]])
    tab <- table(df[[v]][pair], df[[group_col]][pair])
    if (nrow(tab) < 2 || ncol(tab) < 2 || any(tab[tab > 0] < nfilter)) {
      return(list(statistic = NA_real_, df = NA_integer_, p_value = NA_real_, n_total = sum(pair)))
    }
    test <- tryCatch(stats::chisq.test(tab), error = function(e) NULL)
    if (is.null(test)) return(list(statistic = NA_real_, df = NA_integer_, p_value = NA_real_, n_total = sum(pair)))
    list(statistic = unname(test$statistic), df = unname(test$parameter),
         p_value = test$p.value, n_total = sum(pair))
  })
  names(out) <- vars
  out
}

#' @title Follow-up duration per patient, summarized by group (Objective 3)
#' @description Per-patient reduction (max \code{visit_months_col}) happens
#'   entirely server-side; only group-level aggregate stats are returned,
#'   including compliance with a minimum follow-up threshold.
#' @export
d2t_followup_statsDS <- function(df, pat_id_col = "pat_ID",
                                  visit_months_col = "Visit_months_from_diagnosis",
                                  group_col = NULL, min_months = 24, nfilter = 5) {

  per_pat <- stats::aggregate(df[[visit_months_col]], by = list(pat = df[[pat_id_col]]), FUN = max, na.rm = TRUE)
  names(per_pat) <- c("pat_id", "followup")

  if (!is.null(group_col)) {
    grp_lookup <- df[!duplicated(df[[pat_id_col]]), c(pat_id_col, group_col)]
    per_pat[[group_col]] <- grp_lookup[[group_col]][match(per_pat$pat_id, grp_lookup[[pat_id_col]])]
  } else {
    per_pat[["__all__"]] <- "all"
    group_col <- "__all__"
  }

  groups <- unique(per_pat[[group_col]][!is.na(per_pat[[group_col]])])
  out <- lapply(groups, function(g) {
    x <- per_pat$followup[per_pat[[group_col]] == g]
    n <- length(x)
    if (n < nfilter) return(NULL)
    n_meeting <- sum(x >= min_months)
    list(n_patients = n, mean = mean(x), sd = stats::sd(x), min = min(x),
         q25 = stats::quantile(x, 0.25, names = FALSE), median = stats::median(x),
         q75 = stats::quantile(x, 0.75, names = FALSE), max = max(x),
         n_meeting_min = n_meeting, prop_meeting_min = n_meeting / n)
  })
  names(out) <- as.character(groups)
  Filter(Negate(is.null), out)
}

#' @title Inter-visit interval per patient, summarized by group (Objective 4)
#' @description Consecutive differences of \code{visit_months_col} computed
#'   per patient server-side; only pooled group-level stats returned.
#' @export
d2t_visit_interval_statsDS <- function(df, pat_id_col = "pat_ID",
                                        visit_months_col = "Visit_months_from_diagnosis",
                                        group_col = NULL, nfilter = 5) {

  if (is.null(group_col)) { df[["__all__"]] <- "all"; group_col <- "__all__" }

  groups <- unique(df[[group_col]][!is.na(df[[group_col]])])
  out <- lapply(groups, function(g) {
    sub <- df[!is.na(df[[group_col]]) & df[[group_col]] == g, , drop = FALSE]
    n_patients <- length(unique(sub[[pat_id_col]]))
    intervals <- unlist(lapply(split(sub[[visit_months_col]], sub[[pat_id_col]]), function(v) {
      v <- sort(v[!is.na(v)])
      if (length(v) < 2) return(NULL)
      diff(v)
    }))
    if (n_patients < nfilter || length(intervals) < nfilter) return(NULL)
    list(n_patients = n_patients, n_intervals = length(intervals),
         mean = mean(intervals), sd = stats::sd(intervals),
         median = stats::median(intervals),
         q25 = stats::quantile(intervals, 0.25, names = FALSE),
         q75 = stats::quantile(intervals, 0.75, names = FALSE))
  })
  names(out) <- as.character(groups)
  Filter(Negate(is.null), out)
}

#' @title D2T incidence: overall, by cohort, by diagnosis-year bin, by
#'   disease-duration bin (Objective 5)
#' @description Per-patient "ever D2T" reduction happens server-side.
#'   Calendar-year and disease-duration are both simple binned counts (NOT
#'   time-to-event/KM -- see Objective 6 note in run_D2T_exploratory_orchestrator.R).
#' @export
d2t_incidenceDS <- function(df, event_col = "D2T", pat_id_col = "pat_ID",
                             visit_months_col = "Visit_months_from_diagnosis",
                             year_diagnosis_col = "Year_diagnosis",
                             group_col = NULL, year_bin_width = 2, duration_bin_width = 12,
                             nfilter = 5) {

  ever <- stats::aggregate(df[[event_col]], by = list(pat = df[[pat_id_col]]),
                            FUN = function(x) as.integer(any(x == 1, na.rm = TRUE)))
  names(ever) <- c("pat_id", "ever_event")

  .prop <- function(idx) {
    n <- length(idx)
    if (n < nfilter) return(NULL)
    n_event <- sum(ever$ever_event[idx])
    list(n_patients = n, n_ever_event = n_event, proportion = n_event / n)
  }

  overall <- .prop(seq_len(nrow(ever)))

  by_cohort <- NULL
  if (!is.null(group_col)) {
    grp_lookup <- df[!duplicated(df[[pat_id_col]]), c(pat_id_col, group_col)]
    ever[[group_col]] <- grp_lookup[[group_col]][match(ever$pat_id, grp_lookup[[pat_id_col]])]
    groups <- unique(ever[[group_col]][!is.na(ever[[group_col]])])
    by_cohort <- setNames(lapply(groups, function(g) .prop(which(ever[[group_col]] == g))), as.character(groups))
    by_cohort <- Filter(Negate(is.null), by_cohort)
  }

  by_year_bin <- NULL
  if (!is.null(year_diagnosis_col) && year_diagnosis_col %in% names(df)) {
    yr_lookup <- df[!duplicated(df[[pat_id_col]]), c(pat_id_col, year_diagnosis_col)]
    ever$year <- yr_lookup[[year_diagnosis_col]][match(ever$pat_id, yr_lookup[[pat_id_col]])]
    ever$year_bin <- floor(ever$year / year_bin_width) * year_bin_width
    bins <- sort(unique(ever$year_bin[!is.na(ever$year_bin)]))
    by_year_bin <- setNames(lapply(bins, function(b) .prop(which(ever$year_bin == b))), as.character(bins))
    by_year_bin <- Filter(Negate(is.null), by_year_bin)
  }

  by_duration_bin <- NULL
  event_rows <- df[!is.na(df[[event_col]]) & df[[event_col]] == 1, , drop = FALSE]
  if (nrow(event_rows) > 0) {
    first_event <- event_rows[order(event_rows[[pat_id_col]], event_rows[[visit_months_col]]), ]
    first_event <- first_event[!duplicated(first_event[[pat_id_col]]), ]
    dur <- first_event[[visit_months_col]]
    dur_bin <- floor(dur / duration_bin_width) * duration_bin_width
    bins <- sort(unique(dur_bin[!is.na(dur_bin)]))
    counts <- setNames(as.list(table(dur_bin)), as.character(bins))
    counts <- lapply(counts, function(c) if (c < nfilter) NA_integer_ else c)
    by_duration_bin <- counts
  }

  list(overall = overall, by_cohort = by_cohort, by_year_bin = by_year_bin, by_duration_bin = by_duration_bin)
}
