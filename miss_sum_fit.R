################################################################################
## Wrapper to, for a given longitudinal data set:
## 1. Introduce each version of missingness
##    All combinations of:
##    - MCAR, MAR, MNAR
##    - 5%, 20%, 35%, 50% days missing
##    - MAR: Weak, moderate, strong association with covariate
##    - MNAR: Weak, moderate, strong association with exposure
## 2. Summarize the exposure (inc. missingness) in each specified way
##    1. ignore - count only observed exposure
##    2. worst - assume all missing days have exposure
##    3. delete - any subject missing any exposure gets NA (5% missing only),
##       then impute summary value when fitting model
##    4. impute: impute daily values, then summarize each imputation, use those
##       for model fitting
## 3. Fit models to each variation (outcome~summarized exposure); extract B, SE
## All specifications and results are stored in a data.frame.
################################################################################

source("introduce_missing.R")
source("summary_strategies.R")
source("fit_models.R")

library(purrr)
library(dplyr)
library(furrr)
plan(multiprocess)

miss_sum_fit <- function(
  ## Simulated in previous steps:
  df_long,    ## longitudinal df including exposure (and covariate, if MAR)
  df_out_org, ## "matching" df with ID, outcome, true relationship between
              ##   exposure and outcome
  seed_set = 8675309, ## Use same seed for anything involving randomness
  num_imp     = 3,       ## #imputations for summarizing exposure variable
  df_id       ## integer - grouping variable for dataset
){

  ## Introduce all variations of missingness to simulated dataset
  ## dfs_missing should have length = 28
  dfs_missing <- intro_missing(df = df_long, seed_set = seed_set)

  ## Strategy 1: Ignore missingness; exposure = sum(observed)
  mods_ignore <- map(dfs_missing, summarize_ignore) %>%
    map_dfr(fit_lm_typical, dfs_outcome = df_out_org, summary_strat = "ignore")

  ## Strategy 2: Assume missing = has exposure  
  mods_worst <- map(dfs_missing, summarize_worst) %>%
    map_dfr(fit_lm_typical, dfs_outcome = df_out_org, summary_strat = "worst")

  ## Strategy 3: Delete It (then impute)  
  mods_delete <- map(
    dfs_missing,
    poss_summarize_delete,
    seed_set = seed_set, num_imp = num_imp
  ) %>%
    ## Some of our datasets are NULL, because it turns out this method falls apart
    ## when >5% patient-days are missing.
    ## Throw those out, fit the model on the rest.
    discard(is.null) %>%
    map_dfr(fit_lm_impute, dfs_outcome = df_out_org, summary_strat = "delete")
  
  ## Strategy 4: Impute It on the Daily
  mods_impute <- map(
    dfs_missing,
    summarize_impute,
    seed_set = seed_set, num_imp = num_imp
  ) %>%
    map_dfr(fit_lm_impute, dfs_outcome = df_out_org, summary_strat = "impute")
  
  ## Each `mods_xxxx` object is a data.frame, so we just need to combine them
  df_results <- bind_rows(
    mods_ignore, mods_worst, mods_delete, mods_impute
  ) %>%
    mutate(df_id = df_id)
  
  return(df_results)
  
}
