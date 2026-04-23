rm(list = ls())
gc()

pacman::p_load(
  dplyr,
  readr,
  data.table,
  glue
)

source("cleansurvey/config.R")
source("cleansurvey/utils.R")

# Merge parameters
MERGE_PARAMS <- list(
  # Join key
  join_key = "hhid",

  # Join type
  join_type = "inner",

  # Duplicate columns between HH and IND (excluding key)
  # "hh"  = keep HH version (reference source for household variables)
  # "ind" = keep IND version
  # "both"= keep both with suffixes _hh / _ind
  duplicate_cols_strategy = list(
    milieu = "hh",
    region = "hh"
  ),

  # -- Suffixes in case of "both" strategy
  suffixes = c("_hh", "_ind")
)

# Data loading
message("Reading cleaned data...")

df_hh <- read_csv(
  file.path(MAIN_DATA_PATH, "ehcvm2_hh_clean.csv"),
  show_col_types = FALSE
)

df_ind <- read_csv(
  file.path(MAIN_DATA_PATH, "ehcvm2_ind_clean.csv"),
  show_col_types = FALSE
)

# Apply merge functions
df_merged <- run_merge_pipeline(df_hh, df_ind, MERGE_PARAMS)

# Save output data
message("\nSaving...")

fwrite(
  df_merged,
  file.path(MAIN_DATA_PATH, "ehcvm2_merged.csv")
)

message("Merge completed.")
message(glue("   ehcvm2_merged.csv : {nrow(df_merged)} rows x {ncol(df_merged)} columns"))