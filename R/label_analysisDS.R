#' @title label_analysisDS.R -- generic, label-agnostic clinical EDA/inference
#' @description Server-side functions backing \code{run_clinical_label_eda()}
#'   and \code{compare_label_feature_before_after_visit()}. Every function
#'   here is a PLAIN AGGREGATE method (see the package's DATASHIELD manifest
#'   and its disclosure-control note): none of them creates or modifies a
#'   server-side data object, so none needs \code{datashield.assign.expr()}.
#'   Every return value is an aggregate statistic (a count, a correlation
#'   coefficient, a model coefficient, a bin count, a group summary) that
#'   already passed an \code{nfilter} floor where applicable -- no row-level
#'   observation, and no patient identifier, is ever included.
#'
#'   Nothing in this file hard-codes a specific label name (e.g. IP_Score);
#'   every function takes the label/feature/group column NAME as an argument
#'   and works generically on whatever column that name points to, exactly
#'   as the spec requires ("do NOT hard-code IP/EP-specific logic").
#' ---------------------------------------------------------------------------

#' @title List column names of a server-side object
#' @description Column NAMES are structural metadata, not row-level data --
#'   the same thing \code{ds.colnames()} in dsBaseClient/dsBase exposes.
#'   Used by the label-EDA orchestrator to validate that a requested label,
#'   feature, or visit column actually exists before doing anything else.
#' @param df A data frame.
#' @return Character vector of column names.
#' @export
list_columnsDS <- function(df) {
  names(df)
}

#' @title Filter a column list down to numeric columns
#' @description Used by the label-EDA orchestrator to silently drop
#'   non-numeric candidate feature columns before attempting correlation/
#'   model-fitting steps that require numeric input, rather than letting
#'   those steps error out. Column names are structural metadata, not
#'   row-level data.
#' @param df A data frame.
#' @param cols Character vector (or "$"-joined string) of candidate column
#'   names. \code{NULL} (default): every column in \code{df}.
#' @return Character vector, the subset of \code{cols} that are numeric.
#' @export
numeric_columnsDS <- function(df, cols = NULL) {
  candidate <- if (is.null(cols)) names(df) else intersect(cdh_split_cols(cols), names(df))
  candidate[vapply(df[candidate], is.numeric, logical(1))]
}

#' @title Classify a label column's type
#' @description Detects, from the data itself, whether \code{label} looks
#'   continuous, binary, or discrete/ordinal with a manageable number of
#'   levels -- the spec is explicit that no label should be assumed to have
#'   a fixed (e.g. 0-4) scale, so this always inspects the observed range
#'   and number of unique values rather than assuming anything.
#'
#' @param df A data frame.
#' @param label Character, the column name to classify.
#' @param nfilter Minimum non-missing count required to classify at all.
#' @param max_levels_shown Distinct-value threshold at or below which the
#'   actual observed values are returned (safe: a small, bounded set of
#'   score levels like 0:4 is not row-level data); above it, only the count
#'   of distinct values is returned, not the values themselves.
#'
#' @return A list:
#'   \itemize{
#'     \item \code{exists}: logical.
#'     \item \code{n_nonmissing}, \code{n_unique}: counts.
#'     \item \code{is_numeric}: logical.
#'     \item \code{is_integer_valued}: logical (all non-missing values are
#'       whole numbers, to within floating-point tolerance).
#'     \item \code{min}, \code{max}: range (numeric labels only).
#'     \item \code{values}: sorted unique values, only if
#'       \code{n_unique <= max_levels_shown}; \code{NULL} otherwise.
#'     \item \code{suggested_type}: one of \code{"binary"} (exactly 2
#'       levels), \code{"ordinal"} (integer-valued, \code{2 < n_unique <=
#'       max_levels_shown}), or \code{"continuous"} (anything else numeric).
#'       This is a SUGGESTION for the client orchestrator to act on; it is
#'       not itself a statistical decision.
#'   }
#' @export
label_type_infoDS <- function(df, label, nfilter = 5, max_levels_shown = 20) {

  if (!label %in% names(df)) {
    return(list(exists = FALSE))
  }

  x <- df[[label]]
  x_nonmissing <- x[!is.na(x)]
  n_nonmissing <- length(x_nonmissing)

  if (n_nonmissing < nfilter) {
    return(list(exists = TRUE, n_nonmissing = n_nonmissing, below_nfilter = TRUE))
  }

  is_numeric <- is.numeric(x) || (is.factor(x) && all(!is.na(suppressWarnings(as.numeric(as.character(x))))))
  x_num <- if (is.numeric(x)) x_nonmissing else suppressWarnings(as.numeric(as.character(x_nonmissing)))

  n_unique <- length(unique(x_nonmissing))
  is_integer_valued <- is_numeric && all(abs(x_num - round(x_num)) < 1e-8)

  suggested_type <- if (n_unique == 2) {
    "binary"
  } else if (is_numeric && is_integer_valued && n_unique <= max_levels_shown) {
    "ordinal"
  } else {
    "continuous"
  }

  list(
    exists = TRUE,
    n_nonmissing = n_nonmissing,
    n_unique = n_unique,
    is_numeric = is_numeric,
    is_integer_valued = is_integer_valued,
    min = if (is_numeric) min(x_num) else NA_real_,
    max = if (is_numeric) max(x_num) else NA_real_,
    values = if (n_unique <= max_levels_shown) sort(unique(x_nonmissing)) else NULL,
    suggested_type = suggested_type
  )
}

#' @title Distribution summary for one label (or any single column)
#' @description Numeric summary + binned histogram (for the density/
#'   histogram/boxplot figures) and, for columns with a manageable number of
#'   distinct values, a frequency table (for the bar/percentage-table
#'   figures) -- via the package's shared \code{cdh_categorical_summary}
#'   full-vs-extremes disclosure rule.
#'
#' @param df A data frame.
#' @param label Character, the column to summarize.
#' @param num_bins Number of histogram bins. Default 20.
#' @param nfilter Minimum count required to release a statistic/bin/cell.
#' @param max_categories_shown Distinct-value threshold for the frequency
#'   table -- see \code{cdh_categorical_summary}.
#'
#' @return A list with \code{n}, \code{summary} (n, mean, sd, min, q25,
#'   median, q75, max -- the boxplot five-number-summary plus mean/sd),
#'   \code{histogram} (\code{breaks}, \code{counts}), and
#'   \code{frequency_table} (output of \code{cdh_categorical_summary} on the
#'   label's own values -- \code{NULL} if not numeric).
#' @export
label_distributionDS <- function(df, label, num_bins = 20, nfilter = 5,
                                  max_categories_shown = 20) {

  if (!label %in% names(df)) stop("label '", label, "' not found in data")

  x <- df[[label]]
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < nfilter) stop("label '", label, "' has fewer than nfilter non-missing values")

  is_numeric <- is.numeric(x)
  x_num <- if (is_numeric) x else suppressWarnings(as.numeric(as.character(x)))

  summary_stats <- list(
    n = n, mean = mean(x_num, na.rm = TRUE), sd = stats::sd(x_num, na.rm = TRUE),
    min = min(x_num, na.rm = TRUE), q25 = stats::quantile(x_num, 0.25, na.rm = TRUE, names = FALSE),
    median = stats::median(x_num, na.rm = TRUE),
    q75 = stats::quantile(x_num, 0.75, na.rm = TRUE, names = FALSE),
    max = max(x_num, na.rm = TRUE)
  )

  histogram <- NULL
  if (is_numeric) {
    rng <- range(x_num, na.rm = TRUE)
    if (diff(rng) > 0) {
      breaks <- seq(rng[1], rng[2], length.out = num_bins + 1)
      counts <- as.integer(table(cut(x_num, breaks = breaks, include.lowest = TRUE)))
      counts[counts > 0 & counts < nfilter] <- NA
      histogram <- list(breaks = breaks, counts = counts)
    } else {
      histogram <- list(breaks = c(rng[1], rng[1] + 1), counts = n)
    }
  }

  freq <- cdh_categorical_summary(x, nfilter = nfilter, max_categories_shown = max_categories_shown)

  list(n = n, summary = summary_stats, histogram = histogram, frequency_table = freq)
}

# ============================================================================
# BIVARIATE ASSOCIATION: label vs. clinical features
# ============================================================================

#' @title Spearman correlation of one column against several features
#' @description Vectorized (one round trip for every feature) complete-case
#'   Spearman correlation between \code{x_col} (typically the label) and
#'   each of \code{features}. Optionally restricted to a "before"/"after"
#'   period relative to a visit cutoff, for the longitudinal comparison
#'   (\code{compare_label_feature_before_after_visit()}).
#'
#' @param df A data frame.
#' @param x_col Character, the column to correlate every feature against
#'   (typically the label).
#' @param features Character vector (or "$"-joined string) of feature column names.
#' @param nfilter Minimum complete-case n required to release a result for a feature.
#' @param visit_col,visit_cutoff,period Optional: if all three are supplied,
#'   rows are restricted to \code{period} ("before" or "after") of
#'   \code{visit_col} relative to \code{visit_cutoff} (see \code{cdh_period_factor})
#'   before computing anything.
#'
#' @return A list, one entry per feature actually present, each with
#'   \code{n}, \code{rho}, \code{p_value} (all \code{NA} if n < nfilter).
#' @export
spearman_multiDS <- function(df, x_col, features, nfilter = 5,
                              visit_col = NULL, visit_cutoff = NULL, period = NULL) {

  if (!is.null(visit_col) && !is.null(visit_cutoff) && !is.null(period)) {
    if (!visit_col %in% names(df)) stop("visit_col '", visit_col, "' not found in data")
    per <- cdh_period_factor(df, visit_col, visit_cutoff)
    df <- df[!is.na(per) & per == period, , drop = FALSE]
  }

  if (!x_col %in% names(df)) stop("x_col '", x_col, "' not found in data")
  feats <- intersect(cdh_split_cols(features), names(df))

  out <- lapply(feats, function(fc) {
    pair <- stats::complete.cases(df[[x_col]], df[[fc]])
    n <- sum(pair)
    if (n < nfilter) return(list(n = n, rho = NA_real_, p_value = NA_real_))
    test <- tryCatch(
      stats::cor.test(df[[x_col]][pair], df[[fc]][pair], method = "spearman", exact = FALSE),
      error = function(e) NULL
    )
    if (is.null(test)) return(list(n = n, rho = NA_real_, p_value = NA_real_))
    list(n = n, rho = unname(test$estimate), p_value = test$p.value)
  })
  names(out) <- feats
  out
}

#' @title Complete-case summary statistics of one column, grouped
#' @description Generic per-group five-number-summary + n, used for BOTH
#'   "feature distribution by label level" boxplots and (via
#'   \code{period_group_statsDS}, which derives the grouping itself)
#'   before/after-visit boxplots. Groups with fewer than \code{nfilter}
#'   complete-case observations are omitted entirely (not just suppressed),
#'   since an empty/near-empty group's mere presence in a small, disclosed
#'   category set can itself be informative.
#'
#' @param df A data frame.
#' @param value_col Character, the column to summarize.
#' @param group_col Character, the column to group by (e.g. the label
#'   itself, for "feature distribution by label level" use \code{value_col
#'   = feature, group_col = label} -- the roles are swappable, this function
#'   doesn't care which is conceptually the "label").
#' @param nfilter Minimum complete-case n required to release a group.
#'
#' @return A named list, one entry per surviving group level (group values
#'   are score levels or short category labels here, e.g. \code{"0"}..\code{"4"}
#'   or \code{"before"}/\code{"after"} -- not patient-level data), each with
#'   \code{n, mean, sd, min, q25, median, q75, max}.
#' @export
group_summary_statsDS <- function(df, value_col, group_col, nfilter = 5) {

  if (!value_col %in% names(df)) stop("value_col '", value_col, "' not found in data")
  if (!group_col %in% names(df)) stop("group_col '", group_col, "' not found in data")

  pair <- stats::complete.cases(df[[value_col]], df[[group_col]])
  d <- df[pair, , drop = FALSE]
  groups <- unique(d[[group_col]])

  out <- lapply(groups, function(g) {
    x <- d[[value_col]][d[[group_col]] == g]
    n <- length(x)
    if (n < nfilter) return(NULL)
    list(n = n, mean = mean(x), sd = stats::sd(x), min = min(x),
         q25 = stats::quantile(x, 0.25, names = FALSE), median = stats::median(x),
         q75 = stats::quantile(x, 0.75, names = FALSE), max = max(x))
  })
  names(out) <- as.character(groups)
  Filter(Negate(is.null), out)
}

#' @title Kruskal-Wallis test of several features across levels of a group column
#' @description Vectorized complete-case Kruskal-Wallis test, one call for
#'   every feature. A feature is skipped (test not run, \code{NA} returned)
#'   if any group has fewer than \code{nfilter} complete-case observations
#'   for that feature, or if fewer than 2 groups remain.
#'
#' @param df A data frame.
#' @param features Character vector (or "$"-joined string) of feature column names.
#' @param group_col Character, the grouping column (typically the label).
#' @param nfilter Minimum per-group complete-case n required.
#'
#' @return A list, one entry per feature, with \code{statistic}, \code{df},
#'   \code{p_value}, \code{n_total}, \code{group_ns} (named list of
#'   per-group n -- counts only).
#' @export
kruskal_by_groupDS <- function(df, features, group_col, nfilter = 5) {

  if (!group_col %in% names(df)) stop("group_col '", group_col, "' not found in data")
  feats <- intersect(cdh_split_cols(features), names(df))

  out <- lapply(feats, function(fc) {
    pair <- stats::complete.cases(df[[fc]], df[[group_col]])
    d <- df[pair, , drop = FALSE]
    group_ns <- table(d[[group_col]])

    if (length(group_ns) < 2 || any(group_ns < nfilter)) {
      return(list(statistic = NA_real_, df = NA_integer_, p_value = NA_real_,
                  n_total = sum(pair), group_ns = as.list(group_ns)))
    }

    test <- tryCatch(stats::kruskal.test(d[[fc]], as.factor(d[[group_col]])),
                      error = function(e) NULL)
    if (is.null(test)) {
      return(list(statistic = NA_real_, df = NA_integer_, p_value = NA_real_,
                  n_total = sum(pair), group_ns = as.list(group_ns)))
    }

    list(statistic = unname(test$statistic), df = unname(test$parameter),
         p_value = test$p.value, n_total = sum(pair), group_ns = as.list(group_ns))
  })
  names(out) <- feats
  out
}

#' @title Wilcoxon (Mann-Whitney) test of several features between two groups
#' @description Vectorized complete-case Wilcoxon rank-sum test. Requires
#'   \code{group_col} to have exactly 2 non-missing levels overall (checked
#'   by the caller via \code{label_type_infoDS} before calling this); a
#'   feature whose complete-case data ends up with anything other than 2
#'   groups, or a group below \code{nfilter}, is skipped (\code{NA}).
#'
#' @param df A data frame.
#' @param features Character vector (or "$"-joined string) of feature column names.
#' @param group_col Character, the binary grouping column.
#' @param nfilter Minimum per-group complete-case n required.
#'
#' @return A list, one entry per feature, with \code{statistic}, \code{p_value},
#'   \code{group_1_n}, \code{group_2_n}, \code{group_1_label}, \code{group_2_label}.
#' @export
wilcox_binaryDS <- function(df, features, group_col, nfilter = 5) {

  if (!group_col %in% names(df)) stop("group_col '", group_col, "' not found in data")
  feats <- intersect(cdh_split_cols(features), names(df))

  out <- lapply(feats, function(fc) {
    pair <- stats::complete.cases(df[[fc]], df[[group_col]])
    d <- df[pair, , drop = FALSE]
    lv <- sort(unique(d[[group_col]]))

    if (length(lv) != 2) {
      return(list(statistic = NA_real_, p_value = NA_real_,
                  group_1_n = NA_integer_, group_2_n = NA_integer_,
                  group_1_label = NA, group_2_label = NA))
    }

    n1 <- sum(d[[group_col]] == lv[1]); n2 <- sum(d[[group_col]] == lv[2])
    if (n1 < nfilter || n2 < nfilter) {
      return(list(statistic = NA_real_, p_value = NA_real_,
                  group_1_n = n1, group_2_n = n2,
                  group_1_label = as.character(lv[1]), group_2_label = as.character(lv[2])))
    }

    test <- tryCatch(stats::wilcox.test(d[[fc]][d[[group_col]] == lv[1]],
                                         d[[fc]][d[[group_col]] == lv[2]]),
                      error = function(e) NULL)
    if (is.null(test)) {
      return(list(statistic = NA_real_, p_value = NA_real_, group_1_n = n1, group_2_n = n2,
                  group_1_label = as.character(lv[1]), group_2_label = as.character(lv[2])))
    }

    list(statistic = unname(test$statistic), p_value = test$p.value,
         group_1_n = n1, group_2_n = n2,
         group_1_label = as.character(lv[1]), group_2_label = as.character(lv[2]))
  })
  names(out) <- feats
  out
}

# ============================================================================
# MULTIVARIABLE MODELS
# ============================================================================

#' @title Fit a generic multivariable model of a label on clinical features
#' @description Complete-case (across \code{outcome_col} and every column in
#'   \code{predictor_cols}) fit of a linear, logistic, or proportional-odds
#'   model, chosen by \code{model_type} (the orchestrator picks this from
#'   \code{label_type_infoDS()}'s \code{suggested_type} -- this function
#'   itself makes no assumption about what the label "should" be). Only
#'   coefficient-level and whole-model summary statistics are ever returned
#'   -- never fitted values, residuals, or the model object itself -- the
#'   same convention \code{ds.glm}/\code{ds.lm} use elsewhere in DataSHIELD.
#'
#' @param df A data frame.
#' @param outcome_col Character, the outcome (label) column.
#' @param predictor_cols Character vector (or "$"-joined string) of predictor columns.
#' @param model_type One of \code{"lm"} (continuous outcome), \code{"glm"}
#'   (binary outcome, logistic), \code{"polr"} (ordinal outcome, proportional
#'   odds -- requires the \code{MASS} package).
#' @param standardize Logical; if \code{TRUE} (default), numeric predictors
#'   are centered and scaled (mean 0, sd 1) before fitting, so coefficients
#'   are comparable across features on different scales. The return value
#'   always records whether this was done.
#' @param nfilter Minimum complete-case model N required to fit at all.
#'
#' @return A list:
#'   \itemize{
#'     \item \code{model_type}, \code{standardized}, \code{n} (model N).
#'     \item \code{coefficients}: list, one entry per term, with
#'       \code{estimate}, \code{se}, \code{ci_low}, \code{ci_high},
#'       \code{p_value} (and, for \code{glm}, \code{odds_ratio},
#'       \code{or_ci_low}, \code{or_ci_high}).
#'     \item For \code{"lm"}: \code{adj_r_squared}.
#'     \item For \code{"glm"}/\code{"polr"}: \code{aic}.
#'     \item \code{error}: set (and everything else \code{NULL}) if the model
#'       failed to fit (e.g. separation, singular design) -- never a crash.
#'   }
#' @export
multivariable_modelDS <- function(df, outcome_col, predictor_cols,
                                   model_type = c("lm", "glm", "polr"),
                                   standardize = TRUE, nfilter = 5) {

  model_type <- match.arg(model_type)
  if (!outcome_col %in% names(df)) stop("outcome_col '", outcome_col, "' not found in data")
  preds <- intersect(cdh_split_cols(predictor_cols), names(df))
  if (length(preds) == 0) stop("no predictor_cols found in data")

  d <- df[, c(outcome_col, preds), drop = FALSE]
  d <- d[stats::complete.cases(d), , drop = FALSE]
  n <- nrow(d)
  if (n < nfilter) {
    return(list(model_type = model_type, standardized = standardize, n = n,
                error = "model N below disclosure threshold"))
  }

  if (standardize) {
    for (p in preds) {
      if (is.numeric(d[[p]])) {
        d[[p]] <- as.numeric(scale(d[[p]]))
      }
    }
  }

  form <- stats::as.formula(paste(outcome_col, "~", paste(preds, collapse = " + ")))

  fit <- tryCatch({
    if (model_type == "lm") {
      stats::lm(form, data = d)
    } else if (model_type == "glm") {
      d[[outcome_col]] <- as.factor(d[[outcome_col]])
      stats::glm(form, data = d, family = stats::binomial())
    } else {
      if (!requireNamespace("MASS", quietly = TRUE))
        stop("MASS package not available for polr")
      d[[outcome_col]] <- factor(d[[outcome_col]], ordered = TRUE)
      MASS::polr(form, data = d, Hess = TRUE)
    }
  }, error = function(e) e)

  if (inherits(fit, "error")) {
    return(list(model_type = model_type, standardized = standardize, n = n,
                error = conditionMessage(fit)))
  }

  s <- summary(fit)
  ci <- tryCatch(suppressMessages(stats::confint(fit)), error = function(e) NULL)

  if (model_type == "polr") {
    coefs <- s$coefficients
    # polr's coefficient table includes both slope terms and intercept
    # ("|") cutpoints; keep only the slope terms for reporting.
    slope_terms <- setdiff(rownames(coefs), grep("\\|", rownames(coefs), value = TRUE))
    pvals <- 2 * stats::pnorm(abs(coefs[, "t value"]), lower.tail = FALSE)
    coef_list <- lapply(slope_terms, function(tm) {
      est <- coefs[tm, "Value"]; se <- coefs[tm, "Std. Error"]
      list(estimate = est, se = se,
           ci_low = if (!is.null(ci) && tm %in% rownames(ci)) ci[tm, 1] else NA_real_,
           ci_high = if (!is.null(ci) && tm %in% rownames(ci)) ci[tm, 2] else NA_real_,
           p_value = pvals[tm])
    })
    names(coef_list) <- slope_terms
    return(list(model_type = model_type, standardized = standardize, n = n,
                coefficients = coef_list, aic = tryCatch(stats::AIC(fit), error = function(e) NA_real_)))
  }

  coefs <- s$coefficients
  terms <- setdiff(rownames(coefs), "(Intercept)")
  coef_list <- lapply(terms, function(tm) {
    est <- coefs[tm, 1]; se <- coefs[tm, 2]; pv <- coefs[tm, 4]
    entry <- list(estimate = est, se = se,
                  ci_low = if (!is.null(ci) && tm %in% rownames(ci)) ci[tm, 1] else NA_real_,
                  ci_high = if (!is.null(ci) && tm %in% rownames(ci)) ci[tm, 2] else NA_real_,
                  p_value = pv)
    if (model_type == "glm") {
      entry$odds_ratio <- exp(est)
      entry$or_ci_low <- if (!is.null(ci) && tm %in% rownames(ci)) exp(ci[tm, 1]) else NA_real_
      entry$or_ci_high <- if (!is.null(ci) && tm %in% rownames(ci)) exp(ci[tm, 2]) else NA_real_
    }
    entry
  })
  names(coef_list) <- terms

  result <- list(model_type = model_type, standardized = standardize, n = n, coefficients = coef_list)
  if (model_type == "lm") {
    result$adj_r_squared <- s$adj.r.squared
  } else {
    result$aic <- tryCatch(stats::AIC(fit), error = function(e) NA_real_)
  }
  result
}

#' @title Spearman correlation matrix over a label and its clinical features
#' @description Thin wrapper around the shared \code{cdh_correlation_matrix}
#'   helper (also used by \code{exploratory_analysisDS}), restricted to
#'   \code{label} plus \code{features} rather than every numeric column, so
#'   the resulting matrix stays focused and readable.
#'
#' @param df A data frame.
#' @param label Character, the label column (included first in the matrix).
#' @param features Character vector (or "$"-joined string) of feature column names.
#' @param nfilter Minimum pairwise-complete count required to release a cell.
#' @return \code{list(cols=, matrix=)}, or \code{NULL} if fewer than 2 usable
#'   numeric columns.
#' @export
correlation_matrix_labelDS <- function(df, label, features, nfilter = 5) {
  cols <- c(label, intersect(cdh_split_cols(features), names(df)))
  cdh_correlation_matrix(df, cols, nfilter = nfilter, method = "spearman")
}

#' @title Random forest variable importance -- exploratory only
#' @description Complete-case random forest (regression for a continuous
#'   label, classification otherwise) of \code{outcome_col} on
#'   \code{predictor_cols}, reporting ONLY variable importance, OOB
#'   performance, and model N -- never per-observation predictions, OOB
#'   votes, or the fitted trees. Labeled by the caller (see
#'   \code{run_clinical_label_eda}) as exploratory feature ranking, not a
#'   causal or even necessarily predictive claim.
#'
#' @param df A data frame.
#' @param outcome_col Character, the outcome (label) column.
#' @param predictor_cols Character vector (or "$"-joined string) of predictor columns.
#' @param classification Logical; \code{TRUE} fits a classification forest
#'   (outcome coerced to factor), \code{FALSE} (default) regression.
#' @param ntree Number of trees. Default 500.
#' @param nfilter Minimum complete-case model N required to fit at all.
#'
#' @return A list with \code{available} (\code{FALSE} if the \code{randomForest}
#'   package isn't installed), \code{n}, \code{importance} (named list,
#'   feature -> importance value, sorted descending), and either
#'   \code{oob_mse}/\code{pseudo_r_squared} (regression) or
#'   \code{oob_error_rate} (classification).
#' @export
random_forest_importanceDS <- function(df, outcome_col, predictor_cols,
                                        classification = FALSE, ntree = 500, nfilter = 5) {

  if (!requireNamespace("randomForest", quietly = TRUE)) {
    return(list(available = FALSE,
                message = "randomForest package not installed on this server"))
  }

  if (!outcome_col %in% names(df)) stop("outcome_col '", outcome_col, "' not found in data")
  preds <- intersect(cdh_split_cols(predictor_cols), names(df))
  if (length(preds) == 0) stop("no predictor_cols found in data")

  d <- df[, c(outcome_col, preds), drop = FALSE]
  d <- d[stats::complete.cases(d), , drop = FALSE]
  n <- nrow(d)
  if (n < nfilter) {
    return(list(available = TRUE, n = n, error = "model N below disclosure threshold"))
  }

  if (classification) d[[outcome_col]] <- as.factor(d[[outcome_col]])

  fit <- tryCatch(
    randomForest::randomForest(
      x = d[preds], y = d[[outcome_col]], ntree = ntree, importance = TRUE
    ),
    error = function(e) e
  )
  if (inherits(fit, "error")) {
    return(list(available = TRUE, n = n, error = conditionMessage(fit)))
  }

  imp <- randomForest::importance(fit)
  imp_col <- if ("%IncMSE" %in% colnames(imp)) "%IncMSE" else colnames(imp)[1]
  imp_vals <- sort(setNames(imp[, imp_col], rownames(imp)), decreasing = TRUE)

  result <- list(available = TRUE, n = n, importance = as.list(imp_vals))
  if (classification) {
    result$oob_error_rate <- fit$err.rate[nrow(fit$err.rate), "OOB"]
  } else {
    result$oob_mse <- fit$mse[length(fit$mse)]
    result$pseudo_r_squared <- fit$rsq[length(fit$rsq)]
  }
  result
}

# ============================================================================
# LONGITUDINAL: BEFORE vs. AFTER A VISIT CUTOFF
# ============================================================================

#' @title Per-period (before/after visit) summary of one column
#' @description Derives a before/after period from \code{visit_col}/
#'   \code{visit_cutoff} (see \code{cdh_period_factor}) and reports, for
#'   each period, the same n/five-number-summary as
#'   \code{group_summary_statsDS} -- used for both the label's and the
#'   feature's own before/after distribution (boxplots 08/09), and to get
#'   \code{label_mean/median}, \code{feature_mean/median}-style numbers for
#'   the longitudinal comparison table (section 15).
#'
#' @param df A data frame.
#' @param value_col Character, the column to summarize (label or feature).
#' @param visit_col Character, the visit/time column.
#' @param visit_cutoff Numeric cutoff; \code{visit_col < visit_cutoff} is
#'   "before", \code{>=} is "after".
#' @param nfilter Minimum per-period complete-case n required.
#'
#' @return A named list with entries \code{"before"}/\code{"after"} (only
#'   for periods with n >= nfilter), each with \code{n, mean, sd, min, q25,
#'   median, q75, max}.
#' @export
period_group_statsDS <- function(df, value_col, visit_col, visit_cutoff, nfilter = 5) {
  if (!visit_col %in% names(df)) stop("visit_col '", visit_col, "' not found in data")
  df$.cdh_period <- cdh_period_factor(df, visit_col, visit_cutoff)
  group_summary_statsDS(df, value_col = value_col, group_col = ".cdh_period", nfilter = nfilter)
}

#' @title Per-period Spearman correlation between a label and a feature
#' @description Thin, explicitly-named wrapper around
#'   \code{spearman_multiDS}'s period-filtering arguments, returning both
#'   periods' results in one call (two round trips folded into one) for the
#'   longitudinal comparison table (section 15).
#'
#' @param df A data frame.
#' @param label,feature Character, the two columns to correlate.
#' @param visit_col Character, the visit/time column.
#' @param visit_cutoff Numeric period cutoff.
#' @param nfilter Minimum per-period complete-case n required.
#'
#' @return \code{list(before = list(n=,rho=,p_value=), after = list(...))}.
#' @export
period_spearmanDS <- function(df, label, feature, visit_col, visit_cutoff, nfilter = 5) {
  before <- spearman_multiDS(df, x_col = label, features = feature, nfilter = nfilter,
                              visit_col = visit_col, visit_cutoff = visit_cutoff, period = "before")
  after <- spearman_multiDS(df, x_col = label, features = feature, nfilter = nfilter,
                             visit_col = visit_col, visit_cutoff = visit_cutoff, period = "after")
  list(before = before[[feature]], after = after[[feature]])
}

#' @title 2D binned joint histogram of two columns (a disclosure-safe "scatter")
#' @description A true scatterplot of raw (label, feature) pairs would be
#'   row-level data by definition. This function approximates one the same
#'   way \code{semiOPBARTLocalHistogramDS} elsewhere in this codebase
#'   approximates a univariate distribution: shared bin edges on each axis
#'   (built from each column's own observed range), a count per 2D cell,
#'   with any cell below \code{nfilter} suppressed to \code{NA}. Optionally
#'   restricted to a before/after period.
#'
#' @param df A data frame.
#' @param x_col,y_col Character, the two columns (e.g. label, feature).
#' @param num_bins_x,num_bins_y Bins per axis. Default 10 each (2D cells grow
#'   quadratically, so coarser than the 1D histograms elsewhere).
#' @param nfilter Minimum cell count required to release that cell.
#' @param visit_col,visit_cutoff,period Optional period filter, as in
#'   \code{spearman_multiDS}.
#'
#' @return A list with \code{x_breaks}, \code{y_breaks}, and \code{counts}
#'   (a \code{num_bins_x} x \code{num_bins_y} integer matrix, \code{NA} where
#'   suppressed), or \code{NULL} if fewer than \code{nfilter} complete pairs
#'   are available at all.
#' @export
joint_histogram_2dDS <- function(df, x_col, y_col, num_bins_x = 10, num_bins_y = 10,
                                  nfilter = 5, visit_col = NULL, visit_cutoff = NULL,
                                  period = NULL) {

  if (!is.null(visit_col) && !is.null(visit_cutoff) && !is.null(period)) {
    per <- cdh_period_factor(df, visit_col, visit_cutoff)
    df <- df[!is.na(per) & per == period, , drop = FALSE]
  }

  if (!x_col %in% names(df)) stop("x_col '", x_col, "' not found in data")
  if (!y_col %in% names(df)) stop("y_col '", y_col, "' not found in data")

  pair <- stats::complete.cases(df[[x_col]], df[[y_col]])
  x <- df[[x_col]][pair]; y <- df[[y_col]][pair]
  if (length(x) < nfilter) return(NULL)

  x_rng <- range(x); y_rng <- range(y)
  x_breaks <- if (diff(x_rng) > 0) seq(x_rng[1], x_rng[2], length.out = num_bins_x + 1) else c(x_rng[1], x_rng[1] + 1)
  y_breaks <- if (diff(y_rng) > 0) seq(y_rng[1], y_rng[2], length.out = num_bins_y + 1) else c(y_rng[1], y_rng[1] + 1)

  x_bin <- cut(x, breaks = x_breaks, include.lowest = TRUE)
  y_bin <- cut(y, breaks = y_breaks, include.lowest = TRUE)
  counts <- table(x_bin, y_bin)
  counts[counts > 0 & counts < nfilter] <- NA

  list(x_breaks = x_breaks, y_breaks = y_breaks, counts = matrix(
    as.integer(counts), nrow = nrow(counts), dimnames = dimnames(counts)))
}

#' @title Test whether a label-feature association differs before vs. after a visit
#' @description Fits \code{outcome ~ exposure * period} (period derived from
#'   \code{visit_col}/\code{visit_cutoff}), where \code{outcome}/\code{exposure}
#'   are \code{label}/\code{feature} in whichever order the caller specifies
#'   (see \code{outcome_is_label}) -- the spec allows either direction
#'   depending on which variable is conceptually the outcome. Reports the
#'   interaction term only alongside whole-model N; never fitted values.
#'
#' @param df A data frame.
#' @param label,feature Character, the two columns.
#' @param visit_col Character, the visit/time column.
#' @param visit_cutoff Numeric period cutoff.
#' @param outcome_is_label Logical; \code{TRUE} (default) fits
#'   \code{label ~ feature * period}, \code{FALSE} fits
#'   \code{feature ~ label * period}.
#' @param model_type \code{"lm"} (default) or \code{"glm"} (logistic, for a
#'   binary outcome) -- chosen by the caller from \code{label_type_infoDS()}.
#' @param nfilter Minimum complete-case model N required to fit at all.
#'
#' @return A list with \code{formula} (as text, for the record),
#'   \code{n}, and \code{interaction} (\code{estimate}, \code{se},
#'   \code{p_value} for the exposure:period interaction term), or
#'   \code{error} if the model couldn't be fit.
#' @export
interaction_modelDS <- function(df, label, feature, visit_col, visit_cutoff,
                                 outcome_is_label = TRUE,
                                 model_type = c("lm", "glm"), nfilter = 5) {

  model_type <- match.arg(model_type)
  if (!visit_col %in% names(df)) stop("visit_col '", visit_col, "' not found in data")

  df$.cdh_period <- cdh_period_factor(df, visit_col, visit_cutoff)
  outcome_col <- if (outcome_is_label) label else feature
  exposure_col <- if (outcome_is_label) feature else label

  d <- df[, c(outcome_col, exposure_col, ".cdh_period"), drop = FALSE]
  d <- d[stats::complete.cases(d), , drop = FALSE]
  n <- nrow(d)
  if (n < nfilter) return(list(n = n, error = "model N below disclosure threshold"))

  form <- stats::as.formula(paste(outcome_col, "~", exposure_col, "* .cdh_period"))

  fit <- tryCatch({
    if (model_type == "lm") {
      stats::lm(form, data = d)
    } else {
      d[[outcome_col]] <- as.factor(d[[outcome_col]])
      stats::glm(form, data = d, family = stats::binomial())
    }
  }, error = function(e) e)

  if (inherits(fit, "error")) return(list(n = n, error = conditionMessage(fit)))

  coefs <- summary(fit)$coefficients
  inter_term <- grep(":", rownames(coefs), value = TRUE)
  if (length(inter_term) == 0) return(list(n = n, error = "interaction term not estimable"))
  inter_term <- inter_term[1]

  list(
    formula = paste(deparse(form), collapse = ""),
    n = n,
    interaction = list(
      term = inter_term,
      estimate = coefs[inter_term, 1],
      se = coefs[inter_term, 2],
      p_value = coefs[inter_term, 4]
    )
  )
}

#' @title Mixed-effects version of interaction_modelDS, accounting for repeated
#'   measurements within patient
#' @description Same model as \code{interaction_modelDS} but with a random
#'   intercept per patient (\code{lme4::lmer}), so repeated visits from the
#'   same patient aren't treated as independent. Only available for a
#'   continuous outcome (lme4's binomial GLMM support is more fragile with
#'   small per-site N; the caller should fall back to
#'   \code{interaction_modelDS} and clearly report the independence
#'   limitation otherwise, per the spec). \code{pat_ID} values themselves
#'   are never returned -- only the interaction term, model N, and the
#'   number of distinct patients (a count, not identifiers).
#'
#' @param df A data frame.
#' @param label,feature Character, the two columns.
#' @param visit_col Character, the visit/time column.
#' @param visit_cutoff Numeric period cutoff.
#' @param pat_id_col Character, the patient identifier column (grouping only
#'   -- never returned).
#' @param outcome_is_label Logical; see \code{interaction_modelDS}.
#' @param nfilter Minimum complete-case model N (rows) required to fit at all.
#'
#' @return A list with \code{available} (\code{FALSE} if \code{lme4} isn't
#'   installed), \code{n} (rows), \code{n_patients} (distinct patients --
#'   count only), and \code{interaction} (\code{estimate}, \code{se},
#'   \code{p_value} via Wald approximation), or \code{error}.
#' @export
mixed_interaction_modelDS <- function(df, label, feature, visit_col, visit_cutoff,
                                       pat_id_col, outcome_is_label = TRUE, nfilter = 5) {

  if (!requireNamespace("lme4", quietly = TRUE)) {
    return(list(available = FALSE, message = "lme4 package not installed on this server"))
  }
  if (!visit_col %in% names(df)) stop("visit_col '", visit_col, "' not found in data")
  if (!pat_id_col %in% names(df)) stop("pat_id_col '", pat_id_col, "' not found in data")

  df$.cdh_period <- cdh_period_factor(df, visit_col, visit_cutoff)
  outcome_col <- if (outcome_is_label) label else feature
  exposure_col <- if (outcome_is_label) feature else label

  d <- df[, c(outcome_col, exposure_col, ".cdh_period", pat_id_col), drop = FALSE]
  d <- d[stats::complete.cases(d), , drop = FALSE]
  n <- nrow(d)
  n_patients <- length(unique(d[[pat_id_col]]))
  if (n < nfilter) {
    return(list(available = TRUE, n = n, n_patients = n_patients,
                error = "model N below disclosure threshold"))
  }

  form <- stats::as.formula(paste0(outcome_col, " ~ ", exposure_col, " * .cdh_period + (1 | ", pat_id_col, ")"))

  fit <- tryCatch(lme4::lmer(form, data = d), error = function(e) e, warning = function(w) w)
  if (inherits(fit, c("error", "warning"))) {
    return(list(available = TRUE, n = n, n_patients = n_patients,
                error = conditionMessage(fit)))
  }

  coefs <- summary(fit)$coefficients
  inter_term <- grep(":", rownames(coefs), value = TRUE)
  if (length(inter_term) == 0) {
    return(list(available = TRUE, n = n, n_patients = n_patients,
                error = "interaction term not estimable"))
  }
  inter_term <- inter_term[1]
  est <- coefs[inter_term, "Estimate"]; se <- coefs[inter_term, "Std. Error"]
  p_value <- 2 * stats::pnorm(abs(est / se), lower.tail = FALSE)  # Wald approximation

  list(
    available = TRUE, n = n, n_patients = n_patients,
    interaction = list(term = inter_term, estimate = est, se = se, p_value = p_value)
  )
}
