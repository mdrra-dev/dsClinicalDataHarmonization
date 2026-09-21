#' @title genericDataFrameOpsDS.R -- generic select/filter/join/set-value
#' @description Four fully generic building blocks. Each stores its result
#'   under \code{newobj} via \code{base::assign(newobj, ..., envir =
#'   parent.frame())} and returns only a small summary -- called via
#'   \code{datashield.aggregate}, never \code{datashield.assign.expr}.
#' ---------------------------------------------------------------------------

#' @title Select (keep) specific columns
#' @param df A data frame (already resolved).
#' @param columns Character vector (or "$"-joined string) of columns to keep.
#' @param newobj Name under which the result is stored.
#' @return \code{list(newobj=, columns_selected=)}.
#' @export
select_columnsDS <- function(df, columns, newobj) {
  cols <- intersect(cdh_split_cols(columns), names(df))
  df_sel <- df[, cols, drop = FALSE]
  base::assign(newobj, df_sel, envir = parent.frame())
  list(newobj = newobj, columns_selected = cols)
}

# #' @title Filter rows by a simple condition on one column
# #' @param df A data frame (already resolved).
# #' @param column Character, the column to test.
# #' @param operator One of  \code{"=="},  \code{"!="},  \code{">"},  \code{"<"},  \code{">="},  \code{"<="},  \code{"%in%"},  \code{"is.na"},  \code{"not.na"}.
# #' @param value Comparison value; ignored for is.na/not.na.
# #' @param newobj Name under which the result is stored.
# #' @return \code{list(newobj=, n_before=, n_after=)}.
# #'
# #' @export
# filter_rowsDS <- function(df, column, operator = c("==", "!=", ">", "<", ">=", "<=",
#                                                      "%in%", "is.na", "not.na"), value = NULL, newobj) {
#   operator <- match.arg(operator)
#   if (!column %in% names(df)) stop("column '", column, "' not found in data")
#   x <- df[[column]]
#   keep <- switch(operator,
#     "==" = x == value, "!=" = x != value, ">" = x > value, "<" = x < value,
#     ">=" = x >= value, "<=" = x <= value, "%in%" = x %in% value,
#     "is.na" = is.na(x), "not.na" = !is.na(x))
#   keep[is.na(keep)] <- FALSE
#   n_before <- nrow(df)
#   df_filtered <- df[keep, , drop = FALSE]
#   base::assign(newobj, df_filtered, envir = parent.frame())
#   list(newobj = newobj, n_before = n_before, n_after = nrow(df_filtered))
# }
#' @title Filter rows by a simple condition on one column
#'
#' @param df A data frame (already resolved).
#' @param column Character, the column to test.
#' @param operator Text operator: \code{"eq"}, \code{"neq"},
#'   \code{"gt"}, \code{"lt"}, \code{"gte"}, \code{"lte"},
#'   \code{"in"}, \code{"is_na"}, or \code{"not_na"}.
#' @param value Comparison value; ignored for \code{is_na} and \code{not_na}.
#' @param newobj Name under which the result is stored.
#' @return \code{list(newobj=, n_before=, n_after=)}.
#' @export
filter_rowsDS <- function(df,column,operator = c("eq", "neq", "gt", "lt", "gte", 
                                                  "lte", "in", "is_na", "not_na"),value = NULL,newobj) {
operator <- match.arg(operator)
if (!column %in% names(df)) {stop("column '", column, "' not found in data")}
x <- df[[column]]
keep <- switch(
operator,
"eq" = x == value,"neq" = x != value,"gt" = x > value,"lt" = x < value,
"gte" = x >= value,"lte" = x <= value,"in" = x %in% value,"is_na" = is.na(x),"not_na" = !is.na(x)
)
# Treat NA results in the comparison as FALSE.
keep[is.na(keep)] <- FALSE
n_before <- nrow(df)
df_filtered <- df[keep,,drop = FALSE]
base::assign(newobj,df_filtered,envir = parent.frame())
list(newobj = newobj,n_before = n_before,n_after = nrow(df_filtered))
}

#' @title Join two data frames on key column(s), no .x/.y suffixes
#' @description Any non-key column present in BOTH inputs is kept from
#'   \code{df1} only (df2's version dropped before merging) -- deterministic
#'   "left/df1 wins" policy, reported back so it's never silent.
#' @param df1,df2 Data frames (already resolved).
#' @param by Character vector (or "$"-joined string) of shared key column(s).
#' @param join_type One of "inner","left","right","full".
#' @param newobj Name under which the result is stored.
#' @return \code{list(newobj=, n_rows=, overlap_columns_kept_from_df1=)}.
#' @export
join_dataframesDS <- function(df1, df2, by, join_type = c("inner", "left", "right", "full"), newobj) {
  join_type <- match.arg(join_type)
  by_cols <- cdh_split_cols(by)

  missing_keys_1 <- setdiff(by_cols, names(df1))
  missing_keys_2 <- setdiff(by_cols, names(df2))
  if (length(missing_keys_1) > 0) stop("Key column(s) not found in df1: ", paste(missing_keys_1, collapse = ", "))
  if (length(missing_keys_2) > 0) stop("Key column(s) not found in df2: ", paste(missing_keys_2, collapse = ", "))

  overlap <- intersect(setdiff(names(df1), by_cols), setdiff(names(df2), by_cols))
  if (length(overlap) > 0) df2 <- df2[, setdiff(names(df2), overlap), drop = FALSE]

  all.x <- join_type %in% c("left", "full")
  all.y <- join_type %in% c("right", "full")
  merged <- merge(df1, df2, by = by_cols, all.x = all.x, all.y = all.y, suffixes = c("", "_DROP_DUP"))

  dup_cols <- grep("_DROP_DUP$", names(merged), value = TRUE)
  if (length(dup_cols) > 0) merged <- merged[, setdiff(names(merged), dup_cols), drop = FALSE]

  base::assign(newobj, merged, envir = parent.frame())
  list(newobj = newobj, n_rows = nrow(merged), overlap_columns_kept_from_df1 = overlap)
}

#' @title Set (assign) a column's value, generically
#' @description Server-side equivalent of \code{df$newname <- value}.
#' @param df A data frame (already resolved).
#' @param column Character, the column name to set (created if new).
#' @param value A literal value (scalar or vector). Ignored if \code{value_expr} given.
#' @param value_expr Character, R expression evaluated with df's columns in scope.
#' @param newobj Name under which the result is stored.
#' @return \code{list(newobj=, column_set=)}.
#' @export
set_column_valueDS <- function(df, column, value = NULL, value_expr = NULL, newobj) {
  new_val <- if (!is.null(value_expr)) eval(parse(text = value_expr), envir = df) else value
  df[[column]] <- new_val
  base::assign(newobj, df, envir = parent.frame())
  list(newobj = newobj, column_set = column)
}
