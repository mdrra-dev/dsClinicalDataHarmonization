#' @title Check that required and optional variables are present in a data frame
#' @description Verify that a data frame contains every variable in a
#'   \code{required} set, report which \code{optional} variables (checked for
#'   type/range only if present -- see \code{check_numericDS()} /
#'   \code{check_categoricalDS()}) are actually present, and flag anything
#'   else in the data frame as unexpected ("extra"). Backward compatible with
#'   the original single-list behaviour: if \code{optional_string} is empty,
#'   this reduces to the original "everything not required is extra" check.
#'
#' @param df A data frame to be checked.
#' @param variables_string A single character string specifying the required
#'   variable names separated by "$".
#' @param optional_string A single character string specifying optional
#'   variable names separated by "$". Defaults to "" (no optional variables
#'   declared, i.e. legacy behaviour).
#'
#' @return A named list with three elements:
#'   \itemize{
#'     \item \code{missing}: required variables not found in the data frame.
#'     \item \code{optional_present}: declared optional variables that ARE
#'       present in the data frame (informational only, never a failure).
#'     \item \code{extra}: variables found in the data frame that are neither
#'       required nor declared optional.
#'   }
#' @export
#'
check_variablesDS <- function(df, variables_string, optional_string = ""){
  variables <- cdh_split_cols(variables_string)
  optional  <- cdh_split_cols(optional_string)

  missing_df_cols  <- setdiff(variables, colnames(df))               # required but absent
  optional_present <- intersect(optional, colnames(df))              # optional and present
  extra_df_cols    <- setdiff(colnames(df), c(variables, optional))  # neither required nor optional

  return(list(missing = missing_df_cols, optional_present = optional_present, extra = extra_df_cols))
}
