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
summary_strat <- "delete"
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

## -- 4. Simulate outcome | relationship w/ *actual* days delirious ------------
## 4a. Simulate random error
## 4b. Calculate actual days delirious, then linear predictor (= actual score)
rbans_errors <- rnorm(n = npts)

del_rbans_df <- sample_df %>%
  group_by(new_id) %>%
  summarise(
    del_actual = sum(status == "Delirious"),
    mean_sofa = mean(sofa_mod)
  ) %>%
  ungroup()

del_rbans_df$rbans <- int_rbans + bdel * del_rbans_df$del_actual + rbans_errors

## -- 5. Calculate *summary variable* (days of delirium) for each patient, -----
## --    using status with missing values --------------------------------------
if(summary_strat == "ignore"){
  del_miss_df <- sample_df %>%
    group_by(new_id) %>%
    summarise(
      ## Assume all missing days do *not* have the exposure
      del_miss = sum(status_miss == "Delirious", na.rm = TRUE),
      mean_sofa = mean(sofa_mod) ## no missingness here due to study design
    ) %>%
    ## Add simulated RBANS
    left_join(dplyr::select(del_rbans_df, new_id, rbans), by = "new_id")
  
} else if(summary_strat == "worst"){
  del_miss_df <- sample_df %>%
    mutate(
      ## Assume all missing days *do* have the exposure
      status_miss = ifelse(is.na(status_miss), "Delirious", status_miss)
    ) %>%
    group_by(new_id) %>%
    summarise(
      del_miss = sum(status_miss == "Delirious", na.rm = TRUE),
      mean_sofa = mean(sofa_mod) ## no missingness here due to study design
    ) %>%
    ## Add simulated RBANS
    left_join(dplyr::select(del_rbans_df, new_id, rbans), by = "new_id")
  
} else if(summary_strat == "delete"){
  imp_df <- sample_df %>%
    group_by(new_id) %>%
    summarise(
      num_del_miss = sum(is.na(status_miss)),
      del_miss = sum(status_miss == "Delirious", na.rm = TRUE),
      mean_sofa = mean(sofa_mod) ## no missingness here due to study design
    ) %>%
    mutate(
      del_miss = ifelse(num_del_miss > 0, NA, del_miss)
    ) %>%
    dplyr::select(-num_del_miss)
  
  ## Create predictorMatrix for mice(): Need to keep new_id in imp_df so that
  ##  it remains in final dataset, but don't want to use it in imputation
  ##  (it means nothing)
  ## Goal: matrix that is ncol(imp_df) x ncol(imp_df);
  ##       new_id column and diagonal = 0, everything else = 1
  ##       (These are default mice() settings, except for new_id)
  imp_matrix <- matrix(1, nrow = ncol(imp_df), ncol = ncol(imp_df))
  imp_matrix[, match("new_id", names(imp_df))] <- 0
  imp_matrix[match("new_id", names(imp_df)), ] <- 0
  diag(imp_matrix) <- 0
  ## check:
  ## colnames(imp_matrix) <- rownames(imp_matrix) <- names(imp_df); imp_matrix

  ## Use mice to impute *entire* duration of delirium for anyone missing records
  del_mids_1 <- imp_df %>%
    dplyr::select(new_id, del_miss, mean_sofa) %>%
    mice(m = nimp, visitSequence = "monotone", predictorMatrix = imp_matrix)
      ## will need to set seed for reproducibility
  
  ## Merge simulated RBANS for each ID with SOFA, imputed delirium values
  del_comp <- complete(del_mids_1, action = "long", include = TRUE) %>%
    dplyr::select(-.id) %>%
    left_join(dplyr::select(del_rbans_df, new_id, rbans), by = "new_id")
  
  ## Recreate mids() object for modeling in next step
  del_mids <- as.mids(del_comp, .id = "new_id")

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
    dplyr::select(new_id, study_day, matches("sofa|status"), -status) %>%
    ## Status needs to be factor
    mutate_at(
      vars(matches("status")),
      ~ factor(., levels = c("Normal", "Delirious", "Comatose"))
    )
  
  ## Create predictorMatrix for mice(): Need to keep new_id in imp_df so that
  ##  it remains in final dataset, but don't want to use it in imputation
  ##  (it means nothing)
  ## Goal: matrix that is ncol(imp_df) x ncol(imp_df);
  ##       new_id column and diagonal = 0, everything else = 1
  ##       (These are default mice() settings, except for new_id)
  imp_matrix <- matrix(1, nrow = ncol(imp_df), ncol = ncol(imp_df))
  imp_matrix[, match("new_id", names(imp_df))] <- 0
  diag(imp_matrix) <- 0
  ## check:
  ## colnames(imp_matrix) <- rownames(imp_matrix) <- names(imp_df); imp_matrix
  
  del_mice_daily <- mice(
    data = imp_df,
    m = nimp,
    visitSequence = "monotone",
    predictorMatrix = imp_matrix
  ) ## will need to add seed here for reproducibility
  
  del_comp <- complete(del_mice_daily, action = "long", include = TRUE)
  
  ## For each imputation, calculate summary statistic for each patient ID
  del_df <- del_comp %>%
    group_by(.imp, new_id) %>%
    summarise(
      del_miss = sum(status_miss == "Delirious"),
      mean_sofa = mean(sofa_mod)
    ) %>%
    ungroup() %>%
    ## Merge on simulated RBANS values for each patient
    left_join(dplyr::select(del_rbans_df, new_id, rbans), by = "new_id")
  
  ## Create mids() object from del_df to use in modeling
  del_mids <- as.mids(del_df, .id = "new_id")
  
}

## -- 6. Fit model using *delirium with missing data*, extract beta, SD --------
## 6a. Determine model formula based on type of missingness:
##     If MAR, adjust for SOI; otherwise, do not
if(miss_type %in% c("mar")){
  ## MAR: Can adjust for the variable which affects missingness
  mod_formula <- "rbans ~ del_miss + mean_sofa"
} else{
  ## MCAR: Don't need to adjust for variable that affects missingness
  ## MNAR: "Can't" adjust for variable that affects missingness
  mod_formula <- "rbans ~ del_miss"
}

## 6b. Fit model on del_df ("ignore", "worst") or del_mids ("delete", "impute")
if(summary_strat %in% c("ignore", "worst")){
  mod <- eval(parse(text = sprintf("lm(%s, data = del_miss_df)", mod_formula)))
  
  ## Extract beta, SE for delirium from mod
  beta_del <- pluck(mod, "coefficients", "del_miss")
  se_del <- sqrt(vcov(mod)["del_miss", "del_miss"])
  
} else{
  mod <-
    eval(parse(text = sprintf("with(del_mids, lm(formula = %s))", mod_formula)))
  
  ## Extract pooled beta, SE from mod
  mod_pool <- mice::pool(mod)
  
  beta_del <- mod_pool$pooled["del_miss", "estimate"]
  se_del <- mod_pool$pooled["del_miss", "ubar"]
}

## TODO: More code to save all needed info and return in a list
## TODO: Make this script a function for easy purrr-ing