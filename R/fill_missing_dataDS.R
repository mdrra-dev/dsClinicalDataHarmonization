#' @title Fill missing values by patient and visit order
#' @param df A data frame (already resolved).
#' @param pat_id_col,visit_col,value_col Patient id / visit-order / value column names.
#' @param newobj Name under which the result is stored.
#' @return \code{list(newobj=, n_rows=, n_filled=)}.
#' @export
fill_missing_dataDS <- function(df, pat_id_col, visit_col, value_col, newobj) {
  suppressWarnings(suppressPackageStartupMessages({ library(dplyr); library(tidyr) }))

  n_missing_before <- sum(is.na(df[[value_col]]))
  filled_df <- df %>%
    group_by(across(all_of(pat_id_col))) %>%
    arrange(across(all_of(visit_col)), .by_group = TRUE) %>%
    fill(all_of(value_col), .direction = "down") %>%
    ungroup()
  filled_df <- as.data.frame(filled_df)
  n_missing_after <- sum(is.na(filled_df[[value_col]]))

  base::assign(newobj, filled_df, envir = parent.frame())
  list(newobj = newobj, n_rows = nrow(filled_df), n_filled = n_missing_before - n_missing_after)
}
