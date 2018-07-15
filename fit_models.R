################################################################################
## Functions to fit combine simulate outcome with summarized data and fit linear
## regression models, as required by strategy used for summarizing
## "ignore"/"worst": fit a typical linear model
## "delete"/"impute": convert merged data to mids() object, fit using imputation
################################################################################

## In all cases, df_summarized assumed to have components $data (summarized
## data.frame) and $miss_info (metadata about missingness in longitudinal
## dataset that was then summarized)

## -- Prep functions: Extract delirium info from model objects, put in df ------
results_lm <- function(mod, vars = "del_miss"){
  sum_mod <- as.data.frame(
    cbind(
      coef(summary(mod)), ## coefficients, SEs, t statistic, p-value
      mod$df.residual,    ## residual df
      confint(mod)        ## 2.5%, 97.5% confidence limits
    )
  )
  ## Only want row for exposure variable
  del_info <- sum_mod[rownames(sum_mod) %in% vars, ]
  del_info <- set_names(
    del_info,
    c("est_beta", "se_beta", "tvalue", "pvalue", "df", "lcl", "ucl")
  )
  return(
    del_info[, c("est_beta", "se_beta", "tvalue", "df", "pvalue", "lcl", "ucl")]
  )
}

results_mice <- function(mod, vars = "del_miss"){
  sum_mod <- summary(pool(mod), conf.int = TRUE)
  ## Only want row for exposure variable
  del_info <- sum_mod[rownames(sum_mod) %in% vars, ]
  del_info <- set_names(
    del_info,
    c("est_beta", "se_beta", "tvalue", "df", "pvalue", "lcl", "ucl")
  )
  return(del_info)
}

## -- For data summarized using the "ignore" and "worst" strategies ------------
fit_lm_typical <- function(
  df_summarized, ## data.frame; result of summarize_ignore(), summarize_worst()
  dfs_outcome,   ## list of data.frames with simulated outcomes
  summary_strat = c("ignore", "worst") ## add this value to final dataset
){
  ## Merge summarized data onto each set of simulated outcomes
  dfs_merged <- map(
    dfs_outcome, ~ left_join(df_summarized$data, ., by = "new_id")
  )
  
  ## Fit, summarize linear models
  mods <- map(dfs_merged, ~ lm(cogscore ~ del_miss, data = .))
  mod_results <- map_dfr(mods, results_lm) %>%
    ## Add info about missingness scenario, true association
    mutate(
      miss_type = df_summarized$miss_info$miss_type,
      miss_prop = df_summarized$miss_info$miss_prop,
      assoc     = df_summarized$miss_info$assoc,
      strategy  = summary_strat,
      true_beta = map_dbl(dfs_outcome, ~ unique(.$true_beta))
    )
  
  return(mod_results)
  
}

## -- For data summarized using the "delete" or "impute" strategies ------------
fit_lm_impute <- function(
  df_summarized, ## data.frame; result of summarize_ignore(), summarize_worst()
  dfs_outcome,   ## list of data.frames with simulated outcomes
  summary_strat = c("delete", "impute")
){
  ## Deletion not possible when daily missingness >5%; when this happens,
  ## df_summarized is NULL.
  if(is.null(df_summarized)){
    return(NULL)
  }
  
  ## Merge summarized data onto each set of simulated outcomes
  dfs_merged <- map(
    dfs_outcome, ~ left_join(df_summarized$data, ., by = "new_id")
  )
  
  ## Fit, summarize linear models
  mods <- map(dfs_merged, as.mids, .id = "new_id") %>%
    map(~ with(., lm(cogscore ~ del_miss)))
  mod_results <- map_dfr(mods, results_mice) %>%
    ## Add info about missingness scenario, true association
    mutate(
      miss_type = df_summarized$miss_info$miss_type,
      miss_prop = df_summarized$miss_info$miss_prop,
      assoc     = df_summarized$miss_info$assoc,
      strategy  = summary_strat,
      true_beta = map_dbl(dfs_outcome, ~ unique(.$true_beta))
    )
  
  return(mod_results)
  
}
