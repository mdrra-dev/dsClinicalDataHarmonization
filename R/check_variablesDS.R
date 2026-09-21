#' @title Check that required and optional variables are present (core logic)
#' @description Internal core, called both by the registered DS method below
#'   (after it resolves \code{df} from its name) and directly by
#'   \code{harmonization_diagnosisDS} (which already has \code{df} in hand).
#' @export 
.cdh_check_variables_core <- function(df, variables_string, optional_string = "") {
  variables <- cdh_split_cols(variables_string)
  optional  <- cdh_split_cols(optional_string)

  missing_df_cols  <- setdiff(variables, colnames(df))
  optional_present <- intersect(optional, colnames(df))
  extra_df_cols    <- setdiff(colnames(df), c(variables, optional))

  list(missing = missing_df_cols, optional_present = optional_present, extra = extra_df_cols)
}

#' @title Check that required and optional variables are present in a data frame
#' @description Verify that a data frame contains every variable in a
#'   \code{required} set, report which \code{optional} variables are
#'   actually present, and flag anything else as unexpected ("extra").
#'
#' @param df A data frame (already resolved -- DataSHIELD's aggregate dispatch
#'   passes session objects directly, no manual resolution needed).
#' @param variables_string A single character string specifying the required
#'   variable names separated by "$".
#' @param optional_string A single character string specifying optional
#'   variable names separated by "$". Defaults to "".
#'
#' @return A named list with \code{missing}, \code{optional_present}, \code{extra}.
#' @export
check_variablesDS <- function(df, variables_string, optional_string = "") {
  .cdh_check_variables_core(df, variables_string, optional_string)
}
