#' @import dplyr
#' @import mice
#' @title Global (pooled) MICE imputation
#' @param df A data frame (already resolved).
#' @param id_col Identifier column, excluded from imputation, reattached after.
#' @param m,maxit MICE parameters.
#' @param newobj Name under which the result is stored.
#' @return \code{list(newobj=, n_rows=, n_cols=, method_by_column=)}.
#' @export
global_imputationDS <- function(df, id_col, m = 5, maxit = 30, newobj) {
  suppressWarnings(suppressPackageStartupMessages({ library(dplyr); library(mice) }))

  if (id_col %in% names(df)) {
    id_values <- df[[id_col]]; df_work <- df[, names(df) != id_col]
  } else { df_work <- df; id_values <- NULL }

  continuous_vars <- names(df_work)[sapply(df_work, function(x) is.numeric(x) && length(unique(na.omit(x))) > 20)]
  categorical_vars <- setdiff(names(df_work), continuous_vars)

  method_vector <- rep("", ncol(df_work)); names(method_vector) <- names(df_work)
  method_vector[continuous_vars] <- "pmm"
  for (var in categorical_vars) {
    if (any(is.na(df_work[[var]]))) {
      n_unique <- length(unique(na.omit(df_work[[var]])))
      method_vector[var] <- ifelse(n_unique == 2, "logreg", "polyreg")
    }
  }
  for (var in names(method_vector)) {
    if (method_vector[var] %in% c("logreg", "polyreg")) df_work[[var]] <- as.factor(df_work[[var]])
  }

  imp <- mice(df_work, m = m, method = method_vector, predictorMatrix = mice::quickpred(df_work),
              maxit = maxit, printFlag = FALSE, seed = 123)

  imp_list_indexed <- lapply(seq_len(m), function(i) {
    df_imp <- complete(imp, i)
    if (!is.null(id_values)) df_imp[[id_col]] <- id_values
    df_imp$imputation <- i
    df_imp
  })
  df_imp_all <- bind_rows(imp_list_indexed)

  group_col <- if (!is.null(id_values)) id_col else NULL
  df_final <- if (!is.null(group_col)) {
    df_imp_all %>% group_by(.data[[group_col]]) %>%
      summarise(across(all_of(continuous_vars), ~ mean(.x, na.rm = TRUE)),
                across(all_of(categorical_vars), ~ names(sort(table(.x), decreasing = TRUE))[1]),
                .groups = "drop")
  } else {
    df_imp_all %>% summarise(across(all_of(continuous_vars), ~ mean(.x, na.rm = TRUE)),
                              across(all_of(categorical_vars), ~ names(sort(table(.x), decreasing = TRUE))[1]))
  }
  df_final <- as.data.frame(df_final)

  base::assign(newobj, df_final, envir = parent.frame())
  list(newobj = newobj, n_rows = nrow(df_final), n_cols = ncol(df_final), method_by_column = method_vector)
}
