rm(list = ls())
gc()

pacman::p_load(
  dplyr,
  readr,
  stringr,
  forcats,
  data.table,
  glue,
  purrr
)

source("cleansurvey/config.R")
source("cleansurvey/utils.R")

# Cleaning parameters: to adapt to a new dataset, modify only this section.
CLEANING_PARAMS <- list(
  # Numeric variables: domain bounds [min, max]
  # NA is produced if the value is out of bounds (structural outlier)
  numeric_bounds = list(
    age          = c(0, 100),
    age_prem_mar = c(10, 80),
    volhor       = c(1, 4380),   # max hours/year = 52 weeks x 84h (12h/day)
    volhor_sec   = c(1, 4380),
    salaire      = c(0, Inf),
    salaire_sec  = c(0, Inf),
    menage       = c(1, 50),
    numind       = c(1, 100)
  ),

  # Numeric variables: NA imputation method: "median" | "mean" | "zero" | "none"
  # "none" = keep NA (often preferable for variables with structural zeros)
  # A prior step could be to fill structural NA with a special code/value
  numeric_impute = list(
    age          = "mean",
    age_prem_mar = "none",
    volhor       = "none",
    volhor_sec   = "none",
    salaire      = "none",
    salaire_sec  = "none",
    menage       = "median",
    numind       = "median"
  ),

  # -- Categorical variables: NA replacement value: "mode" | "Unknown" | "none"
  categ_impute = list(
    sexe              = "mode",
    milieu            = "mode",
    region            = "mode",
    sit_mat           = "Unknown",
    nationalite       = "Unknown",
    handi_majeur      = "No",    # missing = not reported, assume No
    handi_tout_niveau = "No",
    lien_parente      = "none",
    activ7j           = "none",
    activ12m          = "none",
    diplome           = "none",
    educ_hi           = "none",
    educ_scol         = "none",
    csp               = "none",
    branch_activite   = "none"
  ),

  # Cross-variable consistency rules
  # Each rule is a list: condition (text expression) → action on target
  # Action: "na" = replace with NA | "flag" = create a flag_* column
  consistency_rules = list(
    list(
      label     = "age_prem_mar > age",
      condition = "!is.na(age_prem_mar) & age_prem_mar > age",
      target    = "age_prem_mar",
      action    = "na"
    ),
    list(
      label     = "age < 15 and sit_mat not single",
      condition = "age < 15 & !is.na(sit_mat) & sit_mat != 'Célibataire'",
      target    = "sit_mat",
      action    = "flag"
    ),
    list(
      label     = "age < 5 and activ7j not 'Less than 5 years'",
      condition = "age < 5 & !is.na(activ7j) & activ7j != 'Moins de 5 ans'",
      target    = "activ7j",
      action    = "na"
    ),
    list(
      label     = "salary > 0 and activ7j == Inactive",
      condition = "!is.na(salaire) & salaire > 0 & !is.na(activ7j) & activ7j == 'Inactif'",
      target    = "salaire",
      action    = "flag"
    )
  ),

  # -- Variables to drop entirely (redundant, constants)
  vars_to_drop = c(
    "country"
  )
)

# Data loading
message("Reading data...")

df_hh <- read_csv(
  file.path(MAIN_DATA_PATH, "ehcvm2_hh_renamed.csv"),
  show_col_types = FALSE
)

df_ind <- read_csv(
  file.path(MAIN_DATA_PATH, "ehcvm2_ind_renamed.csv"),
  show_col_types = FALSE
)


# Cleaning HH
df_hh_clean <- run_cleaning_pipeline(
  df        = df_hh,
  params    = CLEANING_PARAMS,
  key_cols  = "hhid",
  label     = "HH"
)

# Cleaning IND
df_ind_clean <- run_cleaning_pipeline(
  df        = df_ind,
  params    = CLEANING_PARAMS,
  key_cols  = c("hhid", "numind"),
  label     = "IND"
)

# Save output datasets

message("\nSaving cleaned files...")

fwrite(
  df_hh_clean,
  file.path(MAIN_DATA_PATH, "ehcvm2_hh_clean.csv")
)

fwrite(
  df_ind_clean,
  file.path(MAIN_DATA_PATH, "ehcvm2_ind_clean.csv")
)

message("Cleaning completed.")
message(glue("   ehcvm2_hh_clean.csv  : {nrow(df_hh_clean)} rows x {ncol(df_hh_clean)} columns"))
message(glue("   ehcvm2_ind_clean.csv : {nrow(df_ind_clean)} rows x {ncol(df_ind_clean)} columns"))