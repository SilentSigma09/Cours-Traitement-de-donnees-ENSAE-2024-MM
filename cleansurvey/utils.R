# This file contains useful general functions
pacman::p_load(
  dplyr
  )

# Function to pply a data dict to a dataframe (renaming, relabelliing, type setting)
apply_var_dictionary <- function(df, dict) {

  stopifnot(all(c("var_orig", "var_new", "type_new", "keep") %in% names(dict)))

  dict <- dict %>%
    filter(var_orig %in% names(df))

  rename_map <- setNames(dict$var_orig, dict$var_new)

  df <- df %>%
    rename(!!!rename_map)

  label_map <- setNames(dict$label_new, dict$var_new)

  for (v in intersect(names(df), names(label_map))) {
    attr(df[[v]], "label") <- label_map[[v]]
  }

  for (i in seq_len(nrow(dict))) {
    v <- dict$var_new[i]
    t <- dict$type_new[i]

    if (!v %in% names(df)) next

    if (t == "factor") {
      df[[v]] <- haven::as_factor(df[[v]], levels = "labels")
    } else if (t == "numeric") {
      df[[v]] <- as.numeric(df[[v]])
    } else if (t == "character") {
      df[[v]] <- as.character(df[[v]])
    }
  }

  vars_keep <- dict %>%
    filter(tolower(keep) == "yes") %>%
    pull(var_new)

  df %>%
    select(any_of(vars_keep))
}

# Function to pply a modality dict to a dataframe (modality labeling)
apply_modality_dictionary <- function(df, dict) {

  vars_to_recode <- intersect(names(df), unique(dict$var_name))

  df %>%
    mutate(
      across(
        all_of(vars_to_recode),
        ~ {
          if (!is.factor(.x)) return(.x)

          d <- dict %>% filter(var_name == cur_column())
          if (nrow(d) == 0) return(.x)

          fct_relabel(
            .x,
            function(lvls) {
              idx <- match(lvls, d$label_init)
              ifelse(
                is.na(idx),
                lvls,
                d$label_new[idx]
              )
            }
          )
        }
      )
    )
}
# Key merge functions
# Resolve duplicate columns before joining according to the chosen strategy
resolve_duplicates <- function(df_hh, df_ind, params) {

  strategies <- params$duplicate_cols_strategy
  vars       <- names(strategies)

  for (v in vars) {
    strat <- strategies[[v]]

    if (strat == "hh") {
      # Drop column from IND, keep HH version
      df_ind <- df_ind %>% select(-any_of(v))
      message(glue("  [duplicates] '{v}' : HH version kept, IND column dropped"))

    } else if (strat == "ind") {
      # Drop column from HH
      df_hh <- df_hh %>% select(-any_of(v))
      message(glue("  [duplicates] '{v}' : IND version kept, HH column dropped"))

    } else if (strat == "both") {
      # Rename in both tables with suffixes
      sfx <- params$suffixes
      df_hh <- df_hh %>% rename(!!paste0(v, sfx[1]) := !!v)
      df_ind <- df_ind %>% rename(!!paste0(v, sfx[2]) := !!v)
      message(glue("  [duplicates] '{v}' : renamed to '{v}{sfx[1]}' (HH) and '{v}{sfx[2]}' (IND)"))
    }
  }

  list(hh = df_hh, ind = df_ind)
}


#' Perform the join and produce a quality report
run_merge <- function(df_hh, df_ind, params) {

  key  <- params$join_key
  type <- params$join_type

  n_hh_before  <- nrow(df_hh)
  n_ind_before <- nrow(df_ind)

  # Check join key before merging
  hh_sans_ind  <- setdiff(df_hh[[key]], df_ind[[key]])
  ind_sans_hh  <- setdiff(df_ind[[key]], df_hh[[key]])

  if (length(hh_sans_ind) > 0)
    message(glue("  [merge] {length(hh_sans_ind)} household(s) with no individual in IND"))
  if (length(ind_sans_hh) > 0)
    message(glue("  [merge] ⚠ {length(ind_sans_hh)} individual(s) with hhid missing from HH"))

  # Join
  df_merged <- switch(type,
    "left"  = df_ind %>% left_join(df_hh,  by = key),
    "inner" = df_ind %>% inner_join(df_hh, by = key),
    stop(glue("Unknown join_type: '{type}'. Use 'left' or 'inner'."))
  )

  # Report
  message(glue("  [merge] {type}_join on '{key}'"))
  message(glue("  [merge] HH  : {n_hh_before} households"))
  message(glue("  [merge] IND : {n_ind_before} individuals"))
  message(glue("  [merge] Result: {nrow(df_merged)} rows x {ncol(df_merged)} columns"))

  # Cardinality check (should remain equal to IND row count for inner/left)
  if (nrow(df_merged) != n_ind_before && type == "inner")
    message(glue("  [merge] ⚠ Warning: {n_ind_before - nrow(df_merged)} individual(s) lost after inner join"))

  df_merged
}

#' Full merge pipeline
run_merge_pipeline <- function(df_hh, df_ind, params) {

  message(paste0("\n", strrep("=", 60)))
  message("Merge HH + IND")
  message(strrep("=", 60))

  # Resolve duplicate columns
  resolved  <- resolve_duplicates(df_hh, df_ind, params)

  # Join
  df_merged <- run_merge(resolved$hh, resolved$ind, params)

  # Reorder columns: identifiers first, then HH vars, then IND vars
  id_cols  <- c(params$join_key, "numind")
  hh_vars  <- setdiff(names(resolved$hh), id_cols)
  ind_vars <- setdiff(names(resolved$ind), c(id_cols, hh_vars))

  col_order <- c(
    intersect(id_cols,  names(df_merged)),
    intersect(hh_vars,  names(df_merged)),
    intersect(ind_vars, names(df_merged))
  )

  df_merged <- df_merged %>% select(all_of(col_order))

  message(glue("\n  Final dataset: {nrow(df_merged)} rows x {ncol(df_merged)} columns"))
  message(glue("  Column order: identifiers | HH vars | IND vars"))

  df_merged
}

# Key Cleaning functions
#' Remove constant columns and those listed in vars_to_drop
#' Scalable: works on any list of columns
drop_vars <- function(df, params) {

  # Explicitly listed columns
  to_drop <- intersect(names(df), params$vars_to_drop)

  # Constant columns (only one unique non-NA value)
  constant_cols <- names(df)[sapply(df, function(x) {
    length(unique(na.omit(x))) == 1
  })]
  constant_cols <- setdiff(constant_cols, names(df)[1])  # keep hhid/id

  all_drop <- unique(c(to_drop, constant_cols))

  if (length(all_drop) > 0) {
    message(glue("  [drop_vars] Removing {length(all_drop)} column(s): ",
                 "{paste(all_drop, collapse=', ')}"))
  }

  df %>% select(-any_of(all_drop))
}

#' Apply domain bounds to numeric variables
#' Out-of-bounds values: NA + counted in report
bound_numeric <- function(df, params) {

  bounds <- params$numeric_bounds
  vars   <- intersect(names(df), names(bounds))
  report <- list()

  for (v in vars) {
    lo <- bounds[[v]][1]
    hi <- bounds[[v]][2]
    n_before <- sum(!is.na(df[[v]]))

    df[[v]] <- if_else(
      !is.na(df[[v]]) & (df[[v]] < lo | df[[v]] > hi),
      NA_real_,
      as.numeric(df[[v]])
    )

    n_after  <- sum(!is.na(df[[v]]))
    n_out    <- n_before - n_after
    if (n_out > 0)
      report[[v]] <- glue("{v} : {n_out} value(s) outside [{lo}, {hi}] → NA")
  }

  if (length(report) > 0) {
    message("  [bound_numeric]")
    walk(report, ~ message("    ", .x))
  }

  df
}

#' Impute NA for numeric variables according to chosen strategy
impute_numeric <- function(df, params) {

  strategies <- params$numeric_impute
  vars       <- intersect(names(df), names(strategies))

  for (v in vars) {
    strat  <- strategies[[v]]
    n_na   <- sum(is.na(df[[v]]))
    if (n_na == 0 || strat == "none") next

    fill_val <- switch(strat,
      "median" = median(df[[v]], na.rm = TRUE),
      "mean"   = mean(df[[v]],   na.rm = TRUE),
      "zero"   = 0,
      NA_real_
    )

    df[[v]] <- if_else(is.na(df[[v]]), fill_val, df[[v]])
    message(glue("  [impute_numeric] {v} : {n_na} NA imputed using {strat} ({round(fill_val,1)})"))
  }

  df
}

#' Impute NA for categorical variables
impute_categ <- function(df, params) {

  strategies <- params$categ_impute
  vars       <- intersect(names(df), names(strategies))

  for (v in vars) {
    strat <- strategies[[v]]
    n_na  <- sum(is.na(df[[v]]))
    if (n_na == 0 || strat == "none") next

    fill_val <- if (strat == "mode") {
      tab <- sort(table(df[[v]]), decreasing = TRUE)
      names(tab)[1]
    } else {
      strat
    }

    df[[v]] <- if_else(is.na(df[[v]]), fill_val, df[[v]])
    message(glue("  [impute_categ] {v} : {n_na} NA imputed using '{fill_val}'"))
  }

  df
}

#' Apply cross-variable consistency rules
#' For each rule: evaluate condition, apply "na" or create a flag
apply_consistency_rules <- function(df, params) {

  rules <- params$consistency_rules

  for (rule in rules) {
    mask <- tryCatch(
      eval(parse(text = rule$condition), envir = df),
      error = function(e) {
        message(glue("  [consistency] Rule '{rule$label}' : error -> {e$message}"))
        return(rep(FALSE, nrow(df)))
      }
    )

    n_flagged <- sum(mask, na.rm = TRUE)
    if (n_flagged == 0) next

    if (rule$action == "na") {
      df[[rule$target]][mask] <- NA
      message(glue("  [consistency] '{rule$label}' : {n_flagged} row(s) → NA on {rule$target}"))
    } else if (rule$action == "flag") {
      flag_col <- paste0("flag_", rule$target)
      df[[flag_col]] <- if (flag_col %in% names(df)) df[[flag_col]] | mask else mask
      message(glue("  [consistency] '{rule$label}' : {n_flagged} row(s) flagged → {flag_col}"))
    }
  }

  df
}

#' Standardize categorical labels (trim whitespace, consistent casing)
#' Scalable: automatically applied to all character/factor columns
normalize_categ <- function(df) {

  char_cols <- names(df)[sapply(df, is.character)]

  df <- df %>%
    mutate(across(
      all_of(char_cols),
      ~ str_squish(str_trim(.x))
    ))

  message(glue("  [normalize_categ] Whitespace normalization on {length(char_cols)} column(s)"))
  df
}


#' Remove duplicates based on the primary key
dedup <- function(df, key_cols) {

  n_before <- nrow(df)
  df       <- df %>% distinct(across(all_of(key_cols)), .keep_all = TRUE)
  n_after  <- nrow(df)
  n_dup    <- n_before - n_after

  if (n_dup > 0)
    message(glue("  [dedup] {n_dup} duplicate(s) removed on ({paste(key_cols, collapse=', ')})"))
  else
    message(glue("  [dedup] No duplicates on ({paste(key_cols, collapse=', ')})"))

  df
}


#' Full cleaning pipeline — orchestrates all steps
#' @param df         raw data.frame (already renamed)
#' @param params     CLEANING_PARAMS list
#' @param key_cols   vector of columns forming the primary key
#' @param label      dataset name for messages ("HH" or "IND")
run_cleaning_pipeline <- function(df, params, key_cols, label = "") {

  message(paste0("\n", strrep("=", 60)))
  message(glue("Cleaning: {label}  ({nrow(df)} rows x {ncol(df)} columns)"))
  message(strrep("=", 60))

  df <- df %>%
    dedup(key_cols)              %>%
    drop_vars(params)            %>%
    normalize_categ()            %>%
    bound_numeric(params)        %>%
    apply_consistency_rules(params) %>%
    impute_numeric(params)       %>%
    impute_categ(params)

  message(glue("\n  Final result: {nrow(df)} rows x {ncol(df)} columns"))
  df
}