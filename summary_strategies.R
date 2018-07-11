################################################################################
## Functions to summarize longitudinal exposure (days of delirium) using various
## strategies to deal with missing individual patient-days.
################################################################################

## All functions assume the following columns are present in the original data,
## which has >=1 record per subject:

## - new_id: usually original ID + replication number
## - status: categorical variable (normal, delirious, comatose) with complete
##   data; we use this to simulate the cognitive score (simulate_cogscore())
## - status_miss: categorical variable (normal, delirious, comatose) with
##   missingness introduced in some way (see `introduce_missing.R)`)
## - sofa_mod: only needed for summarize_impute(). Used to help impute missing
##   daily status.

## Returns a data.frame with columns `new_id`, `del_miss`, `cogscore`, *except*
## for summarize_impute(), which returns a long data.frame with an indicator for
## imputation which can be coerced to a mice::mids object.

library(dplyr)
library(mice)

## -- *Ignore* (assume no del. on missing days); sum all observed delirium -----
summarize_ignore <- function(df){
  df %>%
    group_by(new_id) %>%
    summarise(
      days_avail = sum(!is.na(status_miss)),
      del_miss = sum(status_miss == "Delirious", na.rm = TRUE)
    ) %>%
    mutate(
      del_miss = ifelse(days_avail == 0, 0, del_miss)
    ) %>%
    dplyr::select(-days_avail)
}

## -- *Assume the worst* (assume all missing days are delirious) ---------------
summarize_worst <- function(df){
  df %>%
    mutate(
      ## Assume all missing days *do* have the exposure
      status_miss = ifelse(is.na(status_miss), "Delirious", status_miss)
    ) %>%
    group_by(new_id) %>%
    summarise(
      del_miss = sum(status_miss == "Delirious", na.rm = TRUE)
    )
}

## -- *Delete the patient* (leave summary value completely missing) ------------
## Missing values will be filled in via multiple imputation when model is fit

summarize_delete <- function(df){
  df <- df %>%
    group_by(new_id) %>%
    summarise(
      del_miss = sum(status_miss == "Delirious", na.rm = FALSE)
    )
}

## -- *Impute daily data* (returns long df that can become a mids() object) ----
## Imputing at the lowest hierarchy (?)
summarize_impute <- function(df, seed_set, nimp = 5){
  prep_df <- df %>%
    dplyr::select(new_id, sofa_mod, status_miss) %>%
    ## Mice wants factors, not characters!
    mutate(
      status_miss = factor(
        status_miss, levels = c("Normal", "Delirious", "Comatose")
      )
    )
  
  ## Create predictorMatrix for mice(): Need to keep new_id so that it remains
  ## in final dataset and we can merge with simulated outcome. But don't want to
  ## use it in imputation (it means nothing).
  imp_matrix <- make.predictorMatrix(subset(prep_df, select = -new_id))

  mice_long <- mice(
    data = prep_df,
    m = nimp,
    predictorMatrix = imp_matrix,
    seed = seed_set
  )
  
  mice_long_comp <- complete(mice_long, action = "long", include = TRUE)

  ## For each imputation, calculate summary statistic for each patient ID
  df <- mice_long_comp %>%
    group_by(.imp, new_id) %>%
    summarise(
      del_miss = sum(status_miss == "Delirious")
    ) %>%
    ungroup()
  
  ## This can be turned into a mids() object to use in modeling, a la:
  # df_mids <- as.mids(df, .id = "new_id")
  ## We will do this separately, after merging on simulated outcome
  
  return(df)
  
}

