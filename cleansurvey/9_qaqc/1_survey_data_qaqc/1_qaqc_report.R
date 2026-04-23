rm(list = ls())
gc()

pacman::p_load(rmarkdown, glue, here)

source(here("cleansurvey/config.R"))

# ── 0. Report parameters ───────────────────────────────────────────────────────
#
# All configuration is centralised here.
# To adapt to a new survey: modify only this section.

QAQC_PARAMS <- list(

  # -- Input dataset (output of merge step)
  # Use here() for absolute path — rmarkdown::render() shifts wd to the .Rmd folder
  # so relative paths from config.R would no longer resolve inside the document.
  input_file = here(MAIN_DATA_PATH, "ehcvm2_merged.csv"),

  # -- Survey design
  weight_var  = "hhweight",   # sampling weight column (NULL if unweighted)
  strata_var  = "region",     # stratification variable (NULL if none)
  cluster_var = "grappe",     # PSU / cluster variable  (NULL if none)

  # -- Unique row identifier
  id_vars = c("hhid", "numind"),

  # -- Variable classification for univariate section
  # Variables not listed here are still described in the NA overview
  vars_numeric = c(
    "age", "age_prem_mar", "menage",
    "salaire", "salaire_sec",
    "volhor", "volhor_sec"
  ),

  vars_categ = c(
    "sexe", "milieu", "region",
    "activ7j", "activ12m",
    "diplome", "educ_hi",
    "sit_mat", "lien_parente",
    "nationalite",
    "handi_majeur", "handi_tout_niveau",
    "couvmal", "mal30j", "hospital_12m",
    "bank", "telpor", "acces_internet",
    "moustiq_nuit_derniere"
  ),

  # -- Key EHCVM indicators (bivariate section)
  # Each indicator: list(label, numerator_expr, denominator_filter, breakdown_vars)
  # numerator_expr and denominator_filter are quoted expressions evaluated on the df
  indicators = list(

    list(
      label            = "Employment rate (15-64 yrs)",
      numerator_expr   = "activ7j == 'Occupe'",
      denominator_filter = "age >= 15 & age <= 64",
      breakdown_vars   = c("sexe", "milieu", "region")
    ),

    list(
      label            = "Unemployment rate (15-64 yrs)",
      numerator_expr   = "activ7j == 'Chomeur'",
      denominator_filter = "age >= 15 & age <= 64",
      breakdown_vars   = c("sexe", "milieu")
    ),

    list(
      label            = "Net school enrolment rate (6-14 yrs)",
      numerator_expr   = "scol_20_21 == 'Oui'",
      denominator_filter = "age >= 6 & age <= 14",
      breakdown_vars   = c("sexe", "milieu", "region")
    ),

    list(
      label            = "Literacy rate (15+ yrs)",
      numerator_expr   = "alphabet == 'Oui'",
      denominator_filter = "age >= 15",
      breakdown_vars   = c("sexe", "milieu")
    ),

    list(
      label            = "Health insurance coverage rate",
      numerator_expr   = "couvmal == 'Oui'",
      denominator_filter = "!is.na(couvmal)",
      breakdown_vars   = c("sexe", "milieu", "region")
    ),

    list(
      label            = "Mobile phone ownership rate",
      numerator_expr   = "telpor == 'Oui'",
      denominator_filter = "age >= 15",
      breakdown_vars   = c("sexe", "milieu")
    ),

    list(
      label            = "Internet access rate",
      numerator_expr   = "acces_internet == 'Oui'",
      denominator_filter = "age >= 15",
      breakdown_vars   = c("sexe", "milieu", "region")
    )
  ),

  # -- Consistency checks (evaluated as R expressions on the dataset)
  # Each check: list(label, condition_expr)
  # condition_expr should return TRUE for *problematic* rows
  consistency_checks = list(

    list(
      label          = "Age at first marriage > current age",
      condition_expr = "!is.na(age_prem_mar) & age_prem_mar > age"
    ),
    list(
      label          = "Age < 15 with non-single marital status",
      condition_expr = "age < 15 & !is.na(sit_mat) & sit_mat != 'Célibataire'"
    ),
    list(
      label          = "Positive salary with inactive status",
      condition_expr = "!is.na(salaire) & salaire > 0 & !is.na(activ7j) & activ7j == 'Inactif'"
    ),
    list(
      label          = "Weekly hours > 84 (volhor > 4380/yr)",
      condition_expr = "!is.na(volhor) & volhor > 4380"
    ),
    list(
      label          = "Duplicate individual keys (hhid x numind)",
      condition_expr = "duplicated(paste(hhid, numind))"
    )
  ),

  # -- Report metadata
  report_title    = "EHCVM2 — Data Quality & Descriptive Statistics Report",
  report_subtitle = "QAQC before modelling",
  report_author   = "Data Processing Pipeline",
  output_file     = "qaqc_ehcvm2_report.html"
)

# ── 1. Render the report ───────────────────────────────────────────────────────

message(strrep("=", 60))
message("Rendering QAQC report...")
message(strrep("=", 60))

rmarkdown::render(
  input       = here("cleansurvey/9_qaqc/1_survey_data_qaqc/qaqc_report.Rmd"),
  output_file = QAQC_PARAMS$output_file,
  output_dir  = here("data/output_qaqc"),
  params      = list(qaqc_params = QAQC_PARAMS),
  envir       = new.env(parent = globalenv())
)

message(glue("✅ Report saved to output/{QAQC_PARAMS$output_file}"))
