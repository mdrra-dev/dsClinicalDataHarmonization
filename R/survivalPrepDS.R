#' @title Build a patient-level survival table from visit-level data
#' @description Reduces visit-level data to one row per patient (time,
#'   event, optional entry time for left truncation, optional baseline
#'   covariates) -- the format dsSurvivalClient needs. This package only
#'   does this data-prep step; dsSurvival's own validated implementation
#'   does the actual KM/Cox statistics.
#' @param df A data frame (already resolved).
#' @param pat_id_col,event_col,time_col,entry_time_col,group_col,covariate_cols
#'   See package docs.
#' @param newobj Name under which the result is stored.
#' @return \code{list(newobj=, n_patients=, n_events=)}.
#' @export
build_survival_tableDS <- function(df, pat_id_col = "pat_ID", event_col = "D2T",
                                    time_col = "Visit_months_from_diagnosis",
                                    entry_time_col = NULL, group_col = NULL,
                                    covariate_cols = NULL, newobj) {
  for (nm in c(pat_id_col, event_col, time_col)) {
    if (!nm %in% names(df)) stop("column '", nm, "' not found in data")
  }

  pat_ids <- unique(df[[pat_id_col]])
  rows <- lapply(pat_ids, function(pid) {
    sub <- df[df[[pat_id_col]] == pid, , drop = FALSE]
    sub <- sub[order(sub[[time_col]]), , drop = FALSE]

    event <- as.integer(any(sub[[event_col]] == 1, na.rm = TRUE))
    if (event == 1) {
      onset_rows <- sub[!is.na(sub[[event_col]]) & sub[[event_col]] == 1, , drop = FALSE]
      time_val <- onset_rows[[time_col]][1]
    } else {
      time_val <- max(sub[[time_col]], na.rm = TRUE)
    }

    row <- data.frame(pat_id = pid, time = time_val, event = event, stringsAsFactors = FALSE)
    names(row)[1] <- pat_id_col

    if (!is.null(entry_time_col) && entry_time_col %in% names(df)) {
      ev <- sub[[entry_time_col]][!is.na(sub[[entry_time_col]])]
      row$entry <- if (length(ev) > 0) ev[1] else NA_real_
    }
    if (!is.null(group_col) && group_col %in% names(df)) {
      gv <- sub[[group_col]][!is.na(sub[[group_col]])]
      row[[group_col]] <- if (length(gv) > 0) gv[1] else NA
    }
    for (cv in intersect(covariate_cols, names(df))) {
      cvv <- sub[[cv]][!is.na(sub[[cv]])]
      row[[cv]] <- if (length(cvv) > 0) cvv[1] else NA
    }
    row
  })

  surv_table <- do.call(rbind, rows)
  base::assign(newobj, surv_table, envir = parent.frame())
  list(newobj = newobj, n_patients = nrow(surv_table), n_events = sum(surv_table$event))
}
