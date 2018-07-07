################################################################################
## Simulate data under various conditions:
## - Mental status MCAR, MNAR or MAR in various amounts
## - If MAR/MNAR, with various relationships with daily severity of illness
## - Various associations between delirium days and 12m cognitive scores
################################################################################

## -- CURRENTLY JUST A SKETCH WITH ONE DATASET ---------------------------------
## Eventually this will be a function with arguments for various parameters

library(purrr)
library(furrr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggridges)
library(mice)

## For furrr functions, set plan (see futures package, furrr README)
## Not everything is faster with furrr (eg, first map_dfr()); this is a test :)
plan(multiprocess)

## Read in seed data, get vector of unique IDs
seed_df <- readRDS("rawdata/seeddf.rds")
all_ids <- unique(seed_df$id)

## -- Set parameters for testing -----------------------------------------------
## These will all become arguments to a function
miss_type <- "mar" ## Is missingness MCAR or MAR? Values: mcar, mar, mnar
miss_amt <- 0.2 ## What proportion of days should be missing mental status?
summary_strat <- "ignore"
  ## Which strategy to use to calculate total delirium duration?
  ## "ignore" = assume non-delirious (abstract method 2b)
  ## "worst" = assume delirious (2a)
  ## "delete" = delete entire patient, then impute when modeling (1)
  ## "impute" = impute daily status, using previous/next day + SOI, then
  ##            summarize each imputation and use those dfs when modeling
nimp <- 5 ## Number of imputations, if summary_strat = "impute"
          ## (leaving low for now to save time)
bsoi <- 5 ## Relationship between daily SOI and P(status missing) (3)
bdel <- 0 ## Relationship between total delirium days and 12m scores

## Will probably leave these the same for every scenario, at least for now
npts <- 200 ## Number of subjects to include in each rep
int_soi <- -3 ## Intercept, model for P(missing)
  ## aka, log(P(missing)) for someone with SOI = 0
int_rbans <- 79.7103 ## Intercept, model for outcome
  ## aka, average RBANS for someone with no delirium

## -- 1. Simulate a single dataset ---------------------------------------------
## 1. Take random sample of IDs
sample_ids <- sample(all_ids, size = npts, replace = TRUE)

sample_df <- map_dfr(
  sample_ids, ~ subset(seed_df, id == .),
  .id = "rep"
) %>%
  ## Create *new* ID variable that differentiates original IDs included >1 time
  unite(new_id, id, rep, remove = FALSE)

## -- 2. Calculate P(missingness) for each day, given SOI if not MCAR ----------
if(miss_type %in% c("mar", "mnar")){
  ## MAR, MNAR:
  ## 2a. Simulate random errors: ~ N(0, 1)
  ## 2b. Calculate linear predictor for each record
  ## 2c. Transform to probability scale (less work for my brain)
  miss_errors <- rnorm(n = nrow(sample_df))
  
  lp_miss <- int_soi + bsoi * sample_df$sofa_mod + miss_errors
  sample_df$p_miss <- exp(lp_miss) / (1 + exp(lp_miss))
  
  ## Double check
  ## hist(p_miss)
  
  ## 3. Set days with top X% probabilities to missing
  miss_cutoff <- quantile(sample_df$p_miss, probs = (1 - miss_amt))
  sample_df$status_miss <- with(sample_df, ifelse(p_miss >= miss_cutoff, NA, status))
} else{
  ## MCAR: Set random sample of X% patient-days equal to NA
  which_na <- sample(1:nrow(sample_df), round(nrow(sample_df) * miss_amt))
  sample_df$status_miss <- sample_df$status
  sample_df$status_miss[which_na] <- NA
}

## If MAR/MNAR, may want to make sure distribution of SOFA is as different as
##   specified for missing, non-missing observations
# ggplot(data = sample_df) +
#   aes(x = sofa_mod, y = is.na(status_miss)) +
#   stat_density_ridges(
#     geom = "density_ridges_gradient", calc_ecdf = TRUE, quantile_lines = TRUE,
#     jittered_points = TRUE,
#     position = position_points_jitter(width = 0.25, height = 0),
#     point_shape = '|', point_size = 3, alpha = 0.25
#   )

## -- 4. Calculate *summary variable* (days of delirium) for each patient ------
if(summary_strat == "ignore"){
  del_df <- sample_df %>%
    group_by(new_id) %>%
    summarise(
      del_actual = sum(status == "Delirious"),
      ## Assume all missing days do *not* have the exposure
      del_miss = sum(status_miss == "Delirious", na.rm = TRUE),
      mean_sofa = mean(sofa_mod) ## no missingness here due to study design
    )
} else if(summary_strat == "worst"){
  del_df <- sample_df %>%
    mutate(
      ## Assume all missing days *do* have the exposure
      status_miss = ifelse(is.na(status_miss), "Delirious", status_miss)
    ) %>%
    group_by(new_id) %>%
    summarise(
      del_actual = sum(status == "Delirious"),
      del_miss = sum(status_miss == "Delirious", na.rm = TRUE),
      mean_sofa = mean(sofa_mod) ## no missingness here due to study design
    )
} else if(summary_strat == "delete"){
  del_df <- sample_df %>%
    group_by(new_id) %>%
    summarise(
      del_actual = sum(status == "Delirious"),
      num_del_miss = sum(is.na(status_miss)),
      del_miss = sum(status_miss == "Delirious", na.rm = TRUE),
      mean_sofa = mean(sofa_mod) ## no missingness here due to study design
    ) %>%
    mutate(
      del_miss = ifelse(num_del_miss > 0, NA, del_miss)
    )
} else{
  ## Prepare data for imputation: get status, SOI on previous, next days
  imp_df <- sample_df %>%
    arrange(new_id, study_day) %>%
    group_by(new_id) %>%
    mutate(
      last_status = lag(status),
      next_status = lead(status),
      last_sofa = lag(sofa_mod),
      next_sofa = lead(sofa_mod)
    ) %>%
    ungroup() %>%
    dplyr::select(study_day, matches("sofa|status"), -status) %>%
    ## Status needs to be factor
    mutate_at(
      vars(matches("status")),
      ~ factor(., levels = c("Normal", "Delirious", "Comatose"))
    )

  del_mice <- mice(
    data = imp_df,
    m = nimp,
    visitSequence = "monotone"
  ) ## will need to add seed here for reproducibility
  
  del_comp <- complete(del_mice, action = "long")
  del_comp$new_id <- rep(sample_df$new_id, times = nimp)
  
  ## For each imputation, calculate summary statistic for each patient ID
  del_imp_df <- del_comp %>%
    group_by(.imp, new_id) %>%
    summarise(
      del_miss = sum(status_miss == "Delirious"),
      mean_sofa = mean(sofa_mod)
    ) %>%
    ungroup()
  
  ## Need to create a mids object from this, but the coffee shop is closing
}

## -- 5. Simulate outcome | relationship w/ *actual* days delirious ------------
## 5a. Simulate random error
## 5b. Calculate linear predictor (= actual score)
rbans_errors <- rnorm(n = nrow(del_df))
del_df$rbans <- int_rbans + bdel * del_df$del_actual + rbans_errors

## -- 6. Fit model using *delirium with missing data*, extract beta, SD --------
if(miss_type %in% c("mar")){
  ## MAR: Can adjust for the variable which affects missingness
  mod <- lm(rbans ~ del_miss + mean_sofa, data = del_df)
} else{
  ## MCAR: Don't need to adjust for variable that affects missingness
  ## MNAR: "Can't" adjust for variable that affects missingness
  mod <- lm(rbans ~ del_miss, data = del_df)
}
## Additions needed: *impute* models if specified by summary_strat

## Will add code to extract beta, SE (from which we'll calculate CI)

## More code to save all needed info and return in a list