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

## -- MAR: P(missing) is related to daily severity of illness ------------------
addmiss_mar <- function(
  df, ## data.frame to which we want to introduce missing values
  seed_set, ## seed, for reproducibility
  miss_prop = c(0.05, 0.20, 0.35, 0.50), ## overall proportion missing statuses
  assoc_strength = c("weak", "mod", "strong") ## assoc. between SOI, missingness
){
  ## Set intercept, beta for SOI depending on association strength
  ## Values determined so that distribution of missingness for all simulated dfs
  ##   is roughly normal with center as specified
  int <- case_when(
    assoc_strength == "weak"   & miss_prop == 0.05 ~ -2.95,  #
    assoc_strength == "mod"    & miss_prop == 0.05 ~ -3.45,  #
    assoc_strength == "strong" & miss_prop == 0.05 ~ -4.1,   #
    
    assoc_strength == "weak"   & miss_prop == 0.20 ~ -1.4,   #
    assoc_strength == "mod"    & miss_prop == 0.20 ~ -1.85,  #   
    assoc_strength == "strong" & miss_prop == 0.20 ~ -2.475, #   
    
    assoc_strength == "weak"   & miss_prop == 0.35 ~ -0.65,  #
    assoc_strength == "mod"    & miss_prop == 0.35 ~ -1.125, # 
    assoc_strength == "strong" & miss_prop == 0.35 ~ -1.7,   #
    
    assoc_strength == "weak"   & miss_prop == 0.50 ~ -0.04,  #
    assoc_strength == "mod"    & miss_prop == 0.50 ~ -0.5,   #
    assoc_strength == "strong" & miss_prop == 0.50 ~ -1.0,   #
    TRUE ~ as.numeric(NA)
  )
  bsoi <- case_when(
    assoc_strength == "weak"   ~ 0.01,
    assoc_strength == "mod"    ~ 0.1,
    assoc_strength == "strong" ~ 0.2,
    TRUE ~ as.numeric(NA)
  )
  
  ## Calculate P(missing) for each record,
  ##  based on specified proportion of missingness and relationship with daily SOFA
  lp_miss <- int + (bsoi * df$sofa_mod)
  p_miss <- plogis(lp_miss)

  ## Double check
  ## hist(p_miss)
  
  ## Sample missingness for each day's record with p as calculated above
  set.seed(seed_set)
  is_miss <- rbinom(nrow(df), 1, p_miss)
  df$status_miss <- df$status
  df$status_miss[as.logical(is_miss)] <- NA

  # ## Check
  # mean(is.na(df$status_miss))

  return(df)
  
}

# ## Check
# tmp <- addmiss_mar(
#   simdata_list[[sample(1:1000, size = 1)]],
#   seed_set = 53,
#   miss_amt = 0.2,
#   assoc_strength = "mod"
# )
# mean(is.na(tmp$status_miss))

## -- MNAR: P(missing) is related to delirium itself ---------------------------
addmiss_mnar <- function(
  df, ## data.frame to which we want to introduce missing values
  seed_set, ## seed, for reproducibility
  miss_prop = c(0.05, 0.20, 0.35, 0.50), ## overall proportion missing statuses
  assoc_strength = c("weak", "mod", "strong") ## assoc. between del, missingness
){
  ## Set intercept, beta for delirium depending on association strength
  ## Values determined so that distribution of missingness for all simulated dfs
  ##   is roughly normal with center as specified
  int <- case_when(
    assoc_strength == "weak"   & miss_prop == 0.05 ~ -2.9,   #
    assoc_strength == "mod"    & miss_prop == 0.05 ~ -3.0,   # 
    assoc_strength == "strong" & miss_prop == 0.05 ~ -3.2,   #  
    
    assoc_strength == "weak"   & miss_prop == 0.20 ~ -1.375, #   
    assoc_strength == "mod"    & miss_prop == 0.20 ~ -1.5,   #
    assoc_strength == "strong" & miss_prop == 0.20 ~ -1.7,   #   
    
    assoc_strength == "weak"   & miss_prop == 0.35 ~ -0.635, #  
    assoc_strength == "mod"    & miss_prop == 0.35 ~ -0.725, # 
    assoc_strength == "strong" & miss_prop == 0.35 ~ -0.875, #  
    
    assoc_strength == "weak"   & miss_prop == 0.50 ~ -0.025, #
    assoc_strength == "mod"    & miss_prop == 0.50 ~ -0.125, #      
    assoc_strength == "strong" & miss_prop == 0.50 ~ -0.25,  #  
    TRUE ~ as.numeric(NA)
  )
  bdel <- case_when(
    assoc_strength == "weak"   ~ 0.1,
    assoc_strength == "mod"    ~ 0.5,
    assoc_strength == "strong" ~ 1.0,
    TRUE ~ as.numeric(NA)
  )
  
  ## Calculate P(missing) for each record, based on specified proportion of
  ##  missingness and relationship with daily delirium
  lp_miss <- int + (bdel * (df$status == "Delirious"))
  p_miss <- plogis(lp_miss)
  
  ## Double check
  ## hist(p_miss)
  
  ## Sample missingness for each day's record with p as calculated above
  set.seed(seed_set)
  is_miss <- rbinom(nrow(df), 1, p_miss)
  df$status_miss <- df$status
  df$status_miss[as.logical(is_miss)] <- NA
  
  # ## Check
  # mean(is.na(df$status_miss))
  
  return(df)
  
}

# ## Check
# tmp <- addmiss_mnar(
#   simdata_list[[sample(1:1000, size = 1)]],
#   seed_set = 53,
#   miss_amt = 0.2,
#   assoc_strength = "mod"
# )
# mean(is.na(tmp$status_miss))
