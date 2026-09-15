#' @title Fill missing values by patient and visit order
#' @description For each patient, sorts visits by time from diagnosis and
#'   propagates non-missing values of a specified column forward
#'   chronologically within the same patient. Disclosure-safe: the filled
#'   data frame is stored server-side via \code{base::assign(newobj, ..., envir =
#'   parent.frame())} and never appears in this function's own return value.
#'
#' @param df A data frame containing at least the patient identifier column,
#'   the visit time column, and the variable to be filled.
#' @param pat_id_col A character string specifying the name of the
#'   patient identifier column.
#' @param visit_col A character string specifying the name of the
#'   visit time column.
#' @param value_col A character string specifying the name of the
#'   column whose missing values should be filled.
#' @param newobj Name under which the filled data frame is stored.
#'
#' @return A list with \code{newobj}, \code{n_rows}, and \code{n_filled}
#'   (count of previously-missing \code{value_col} cells that received a
#'   carried-forward value -- an aggregate count, non-disclosive).
#' @export
fill_missing_dataDS <- function(df, pat_id_col, visit_col, value_col,
                                 newobj = "fill_missing_data_result") {

  suppressWarnings(suppressPackageStartupMessages({
    library(dplyr)
    library(tidyr)
  }))

  n_missing_before <- sum(is.na(df[[value_col]]))

  filled_df <- df %>%
    group_by(across(all_of(pat_id_col))) %>%
    arrange(across(all_of(visit_col)), .by_group = TRUE) %>%
    fill(all_of(value_col), .direction = "down") %>%
    ungroup()

  filled_df <- as.data.frame(filled_df)
  n_missing_after <- sum(is.na(filled_df[[value_col]]))

  base::assign(newobj, filled_df, envir = parent.frame())

  list(newobj = newobj, n_rows = nrow(filled_df),
       n_filled = n_missing_before - n_missing_after)
}
