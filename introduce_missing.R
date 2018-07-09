################################################################################
## Functions to introduce given types, amounts of missingness into our
## simulated datasets (created in simulate_data.R)
################################################################################

## All functions assume that `df` includes the following columns:
## - new_id (subject + replication ID)
## - status (mental status, complete data)
## - sofa_mod (severity of illness; used to calculate P(missing) if MAR/MNAR)
## All functions 
## 1. Add to `df` a column `status_miss`, which is equal to `status` except for
##    records made missing as specified
## 2. Return `df` + new `status_miss` column

## -- MCAR: Set random sample of miss_amt% patient-days equal to NA ------------
addmiss_mcar <- function(df, seed_set, miss_amt = 0.05){
  ## Set random sample of X% patient-days equal to NA
  set.seed(seed_set)
  which_na <- sample(1:nrow(df), round(nrow(df) * miss_amt))
  df$status_miss <- df$status
  df$status_miss[which_na] <- NA
  
  return(df)
}

## -- MAR/MNAR: P(missing) is related to daily severity of illness -------------
## bsoi: beta for linear relationship between one-unit increase in daily SOFA
##       and log(missing)
## intsoi = -3: default set so that E(P(miss)) for someone with SOFA = 0 is 5%
addmiss_mnar <- function(df, seed_set, bsoi, miss_amt = 0.05, intsoi = -3){
  ## Calculate P(missing) for each record,
  ##  based on specified relationship with daily SOFA
  lp_miss <- intsoi + bsoi * df$sofa_mod
  p_miss <- exp(lp_miss) / (1 + exp(lp_miss))
  
  ## Double check
  ## hist(p_miss)
  
  ## Sample missingness for each day's record with p as calculated above
  is_miss <- rbinom(nrow(df), 1, p_miss)
  df$status_miss <- df$status
  df$status_miss[as.logical(is_miss)] <- NA
  
  ## But how to incorporate **amount** missing?
  
  ## OLD WAY: Set days with top X% probabilities to missing
  ## (No variability for patients with the same SOFA, though)
  # miss_cutoff <- quantile(sample_df$p_miss, probs = (1 - miss_amt))
  # sample_df$status_miss <- with(sample_df, ifelse(p_miss >= miss_cutoff, NA, status))
  
  return(df)
  
}