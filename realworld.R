################################################################################
## Analyze real-world studies according to different methods
## (Leave out "delete"/impute summary value; it stinks)
################################################################################

library(tidyverse)
library(mice)

## Names for strategies
strat_names <- c("NA = unexposed", "NA = exposed", "Impute daily")

## -- Read in data, source functions -------------------------------------------
study1 <- readRDS("rawdata/study1.rds")
study2 <- readRDS("rawdata/study2.rds")
source("fit_models.R")

## Goal for all analyses: Simple models for demo purposes, ignoring covariates

## -- Study 1 (BRAIN-ICU): Very little missing data ----------------------------
study1_ignore <- study1 %>%
  group_by(id, outcome) %>%
  summarise(
    days_avail = sum(!is.na(status)),
    del_days = sum(status == "Delirious", na.rm = TRUE),
    mean_sofa = mean(sofa_mod, na.rm = TRUE)
  ) %>%
  mutate(
    del_days = ifelse(days_avail == 0, 0, del_days)
  ) %>%
  dplyr::select(-days_avail)

study1_worst <- study1 %>%
  ## Assume all missing days *do* have the exposure
  mutate(status = ifelse(is.na(status), "Delirious", status)) %>%
  group_by(id, outcome) %>%
  summarise(
    mean_sofa = mean(sofa_mod, na.rm = TRUE),
    del_days = sum(status == "Delirious", na.rm = TRUE)
  )

study1_sub <- study1 %>%
  dplyr::select(id, sofa_mod, status) %>%
  mutate(
    status = factor( status, levels = c("Normal", "Delirious", "Comatose"))
  )

## Only want to impute with SOFA, status; leave id out of it, but we need to
##  keep it in the data.frame for summarizing and merging
imp_matrix <- make.predictorMatrix(study1_sub)
imp_matrix["id", ] <- 0
imp_matrix[, "id"] <- 0

study1_mice <- mice(
  study1_sub, predictorMatrix = imp_matrix, seed = 8675309, m = 15
)

## Create "complete" (long) version, including original data, which can be
## turned back into a mice object once merged with simulated outcome
study1_comp <- complete(study1_mice, action = "long", include = TRUE) %>%
  group_by(.imp, id) %>%
  summarise(
    mean_sofa = mean(sofa_mod, na.rm = TRUE),
    del_days = sum(status == "Delirious")
  ) %>%
  ungroup() %>%
  left_join(unique(subset(study1, select = c(id, outcome))), by = "id")

## Fit models
mod_ignore_1 <- lm(outcome ~ mean_sofa + del_days, data = study1_ignore)
mod_worst_1 <- lm(outcome ~ mean_sofa + del_days, data = study1_worst)
mod_impute_1 <- with(
  as.mids(study1_comp, .id = "id"), lm(outcome ~ mean_sofa + del_days)
)

results_ignore_1 <- results_lm(mod_ignore_1, vars = "del_days")
results_worst_1 <- results_lm(mod_worst_1, vars = "del_days")
results_impute_1 <- results_mice(mod_impute_1, vars = "del_days")
results_1 <- bind_rows(results_ignore_1, results_worst_1, results_impute_1) %>%
  add_column(strategy = strat_names, .before = "est_beta") %>%
  add_column(
    study = sprintf(
      "BRAIN-ICU\n(%s%% Days Missing)", round(mean(is.na(study1$status)) * 100)
    )
  )

## -- Study 2 (unpublished): Lots of missing data ------------------------------
study2_ignore <- study2 %>%
  group_by(id, outcome) %>%
  summarise(
    days_avail = sum(!is.na(status)),
    del_days = sum(status == "Delirious", na.rm = TRUE),
    mean_sofa = mean(sofa_mod, na.rm = TRUE)
  ) %>%
  mutate(
    del_days = ifelse(days_avail == 0, 0, del_days)
  ) %>%
  dplyr::select(-days_avail)

study2_worst <- study2 %>%
  ## Assume all missing days *do* have the exposure
  mutate(status = ifelse(is.na(status), "Delirious", status)) %>%
  group_by(id, outcome) %>%
  summarise(
    mean_sofa = mean(sofa_mod, na.rm = TRUE),
    del_days = sum(status == "Delirious", na.rm = TRUE)
  )

study2_sub <- study2 %>%
  dplyr::select(id, sofa_mod, status) %>%
  mutate(
    status = factor(status, levels = c("Normal", "Delirious", "Comatose"))
  )

## Only want to impute with SOFA, status; leave id out of it, but we need to
##  keep it in the data.frame for summarizing and merging
imp_matrix <- make.predictorMatrix(study2_sub)
imp_matrix["id", ] <- 0
imp_matrix[, "id"] <- 0

study2_mice <- mice(
  study2_sub, predictorMatrix = imp_matrix, seed = 8675309, m = 15
)

## Create "complete" (long) version, including original data, which can be
## turned back into a mice object once merged with simulated outcome
study2_comp <- complete(study2_mice, action = "long", include = TRUE) %>%
  group_by(.imp, id) %>%
  summarise(
    mean_sofa = mean(sofa_mod, na.rm = TRUE),
    del_days = sum(status == "Delirious")
  ) %>%
  ungroup() %>%
  left_join(unique(subset(study2, select = c(id, outcome))), by = "id")

## Fit models
mod_ignore_2 <- lm(outcome ~ mean_sofa + del_days, data = study2_ignore)
mod_worst_2 <- lm(outcome ~ mean_sofa + del_days, data = study2_worst)
mod_impute_2 <- with(
  as.mids(study2_comp, .id = "id"), lm(outcome ~ mean_sofa + del_days)
)

results_ignore_2 <- results_lm(mod_ignore_2, vars = "del_days")
results_worst_2 <- results_lm(mod_worst_2, vars = "del_days")
results_impute_2 <- results_mice(mod_impute_2, vars = "del_days")
results_2 <- bind_rows(results_ignore_2, results_worst_2, results_impute_2) %>%
  add_column(strategy = strat_names, .before = "est_beta") %>%
  add_column(
    study = sprintf(
      "EMR Study\n(%s%% Days Missing)", round(mean(is.na(study2$status)) * 100)
    )
  )

## -- Combine study results, save ----------------------------------------------
rw_results <- bind_rows(results_1, results_2)
saveRDS(rw_results, "results/realworld.rds")