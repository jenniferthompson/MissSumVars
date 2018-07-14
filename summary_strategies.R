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

## -- *Ignore* (assume no del. on missing days); sum all observed delirium -----
summarize_ignore <- function(df){
  ## Extract missingness info
  miss_info_names <- c("miss_type", "miss_prop", "assoc")
  miss_info <-
    set_names(map(miss_info_names, ~ unique(df[[.]])), miss_info_names)

  df <- df %>%
    group_by(new_id) %>%
    summarise(
      days_avail = sum(!is.na(status_miss)),
      del_miss = sum(status_miss == "Delirious", na.rm = TRUE)
    ) %>%
    mutate(
      del_miss = ifelse(days_avail == 0, 0, del_miss)
    ) %>%
    dplyr::select(-days_avail)

  return(list(data = df, miss_info = miss_info))
  
}

## -- *Assume the worst* (assume all missing days are delirious) ---------------
summarize_worst <- function(df){
  ## Extract missingness info
  miss_info_names <- c("miss_type", "miss_prop", "assoc")
  miss_info <-
    set_names(map(miss_info_names, ~ unique(df[[.]])), miss_info_names)
  
  df <- df %>%
    mutate(
      ## Assume all missing days *do* have the exposure
      status_miss = ifelse(is.na(status_miss), "Delirious", status_miss)
    ) %>%
    group_by(miss_type, miss_prop, assoc, new_id) %>%
    summarise(
      del_miss = sum(status_miss == "Delirious", na.rm = TRUE)
    )

  return(list(data = df, miss_info = miss_info))
  
}

## -- *Delete the patient* (leave summary value completely missing) ------------
## Missing values will be filled in via multiple imputation when model is fit
## NOTE: This strategy is unreliable at >5% missing (even 5% is questionable!)
## 5% yields ~45% patients with missing values

summarize_delete <- function(df, seed_set, num_imp = 3){
  
  ## Extract missingness info
  miss_info_names <- c("miss_type", "miss_prop", "assoc")
  miss_info <-
    set_names(map(miss_info_names, ~ unique(df[[.]])), miss_info_names)
  
  ## Deletion doesn't work if miss_prop > 0.05; stop, issue error
  ## Will use this with purrr::possibly(), where otherwise = NULL
  if(miss_info$miss_prop > 0.05){
    stop("Deletion is not an option when daily missingness > 5%", call. = FALSE)
  }
  
  ## Data prep
  df_sub <- df %>%
    dplyr::select(new_id, sofa_mod, status_miss) %>%
    group_by(new_id) %>%
    summarise(
      mean_sofa = mean(sofa_mod),
      del_miss = sum(status_miss == "Delirious")
    ) %>%
    ungroup()
  
  ## Only want to impute with SOFA, status; leave new_id out of it, but we need
  ## to keep it in the data.frame for summarizing and merging
  imp_matrix <- make.predictorMatrix(df_sub)
  imp_matrix["new_id", ] <- 0
  imp_matrix[, "new_id"] <- 0
  
  df_mice <- mice(
    df_sub, predictorMatrix = imp_matrix, seed = seed_set, m = num_imp
  )
  
  ## Create "complete" (long) version, including original data, which can be
  ## turned back into a mice object once merged with simulated outcome
  df_comp <- complete(df_mice, action = "long", include = TRUE)
  
  ## This can be turned into a mids() object to use in modeling, a la:
  # df_mids <- as.mids(df_comp, .id = "new_id")
  ## We will do this separately, after merging on simulated outcome
  
  return(list(data = df_comp, miss_info = miss_info))
  
}

## Create possibly() version - don't want to run this when proportion of
##  missingness is >5%, but we don't want errors either
poss_summarize_delete <- possibly(summarize_delete, otherwise = NULL)

## -- *Impute daily data* (returns long df that can become a mids() object) ----
## Imputing at the lowest hierarchy (?)
summarize_impute <- function(df, seed_set, num_imp = 3){
  
  ## Extract missingness info
  miss_info_names <- c("miss_type", "miss_prop", "assoc")
  miss_info <-
    set_names(map(miss_info_names, ~ unique(df[[.]])), miss_info_names)
  
  ## Data prep
  df_sub <- df %>%
    dplyr::select(new_id, sofa_mod, status_miss) %>%
    mutate(
      status_miss = factor(
        status_miss, levels = c("Normal", "Delirious", "Comatose")
      )
    )
  
  ## Only want to impute with SOFA, status; leave new_id out of it, but we need
  ## to keep it in the data.frame for summarizing and merging
  imp_matrix <- make.predictorMatrix(df_sub)
  imp_matrix["new_id", ] <- 0
  imp_matrix[, "new_id"] <- 0
  
  df_mice <- mice(
    df_sub, predictorMatrix = imp_matrix, seed = seed_set, m = num_imp
  )

  ## Create "complete" (long) version, including original data, which can be
  ## turned back into a mice object once merged with simulated outcome
  df_comp <- complete(df_mice, action = "long", include = TRUE) %>%
    group_by(.imp, new_id) %>%
    summarise(
      mean_sofa = mean(sofa_mod),
      del_miss = sum(status_miss == "Delirious")
    ) %>%
    ungroup()
  
  ## This can be turned into a mids() object to use in modeling, a la:
  # df_mids <- as.mids(df_comp, .id = "new_id")
  ## We will do this separately, after merging on simulated outcome
  
  return(list(data = df_comp, miss_info = miss_info))
  
}
