################################################################################
## Functions to fit combine simulate outcome with summarized data and fit linear
## regression models, as required by strategy used for summarizing
## "ignore"/"worst": fit a typical linear model
## "delete"/"impute": convert merged data to mids() object, fit using imputation
################################################################################

## In all cases, df_summarized assumed to have components $data (summarized
## data.frame) and $miss_info (metadata about missingness in longitudinal
## dataset that was then summarized)

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
  
  ## Fit linear models
  mods <- map(dfs_merged, ~ lm(cogscore ~ del_miss, data = .))
  
  ## Create data.frame of final results
  miss_info <- bind_rows(
    replicate(length(mods), df_summarized$miss_info, simplify = FALSE)
  )
  mod_results <- bind_cols(
    list(
      strategy = rep(summary_strat, length(dfs_outcome)),
      true_beta = map_dbl(dfs_outcome, ~ unique(.$true_beta)),
      est_beta = map_dbl(mods, ~ pluck(coef(.), "del_miss")),
      se_beta = map_dbl(mods, ~ sqrt(vcov(.)["del_miss", "del_miss"]))
    )
  )
  
  return(
    bind_cols(miss_info, mod_results)
  )
  
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
  
  ## Fit linear models
  mods <- map(dfs_merged, as.mids, .id = "new_id") %>%
    map(~ with(., lm(cogscore ~ del_miss))) %>%
    map(mice::pool)

  ## Create data.frame of final results
  miss_info <- bind_rows(
    replicate(length(mods), df_summarized$miss_info, simplify = FALSE)
  )
  
  mod_results <- bind_cols(
    list(
      strategy = rep(summary_strat, length(dfs_outcome)),
      true_beta = map_dbl(dfs_outcome, ~ unique(.$true_beta)),
      est_beta = map_dbl(mods, pluck, "pooled", "estimate", 2),
      se_beta = map_dbl(mods, pluck, "pooled", "t", 2) %>% sqrt()
    )
  )

  return(
    bind_cols(miss_info, mod_results)
  )
  
}
