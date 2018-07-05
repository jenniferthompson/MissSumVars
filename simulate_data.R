################################################################################
## Simulate data under various conditions:
## - Mental status MCAR or MAR in various amounts
## - If MAR, with various relationships with daily severity of illness
## - Various associations between delirium days and 12m cognitive scores
################################################################################

## -- CURRENTLY JUST A SKETCH WITH ONE DATASET ---------------------------------
## Eventually this will be a function with arguments for various parameters

library(purrr)
library(furrr)
library(dplyr)
library(tidyr)

## For furrr functions, set plan (see futures package, furrr README)
## Not everything is faster with furrr (eg, first map_dfr()); this is a test :)
plan(multiprocess)

## Read in seed data, get vector of unique IDs
seed_df <- readRDS("rawdata/seeddf.rds")
all_ids <- unique(seed_df$id)

## -- Set parameters for testing -----------------------------------------------
## These will all become arguments to a function
miss_type <- "mar" ## Is missingness MCAR or MAR?
miss_amt <- 0.2 ## What proportion of days should be missing mental status?
## summary_strat - which strategy to use to calculate total delirium duration
## ie, assume non-delirious (current), assume delirious, delete entire patient,
##  or use complicated mice() strategy
bsoi <- 0 ## Relationship between daily SOI and P(status missing)
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
  sample_ids, ~ subset(seed_df, id == .)
)

## ****SIREN****
## Need to create a *new* ID variable for grouping later, so that if Pt 1 is
## included 3x in sample, their delirium duration isn't tripled!

## -- 2. Calculate P(missingness) for each day, given SOI ----------------------
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

## -- 4. Calculate *summary variable* (days of delirium) for each patient ------
## FOR NOW, calculating "missing" version by ignoring missing days (=not delirious).
##  Later, will write code to calculate del_miss using our various scenarios
##  (ie, function will have several "if(){...} else{...}" statements which will
##  depend on summary_strat argument)
del_df <- sample_df %>%
  group_by(id) %>% ## THIS NEEDS TO BE FIXED - see above!
  summarise(
    del_actual = sum(status == "Delirious"),
    del_miss = sum(status_miss == "Delirious", na.rm = TRUE)
  )

## -- 5. Simulate outcome | relationship w/ *actual* days delirious ------------
## 5a. Simulate random error
## 5b. Calculate linear predictor (= actual score)
rbans_errors <- rnorm(n = nrow(del_df))
del_df$rbans <- int_rbans + bdel * del_df$del_actual + rbans_errors

## -- 6. Fit model using *delirium with missing data*, extract beta, SD --------
mod <- lm(rbans ~ del_miss, data = del_df)
## code to extract beta, SD (from which we'll calculate CI)

## More code to save all needed info and return in a list