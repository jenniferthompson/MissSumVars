################################################################################
## Function to simulate cognitive scores based on *actual* days delirious in
## a given data.frame, given a specified relationship between delirium/RBANS
################################################################################

## Assumes `df` has columns `new_id` (subject + rep ID) and `status` (which
## takes values Normal, Delirious, Comatose) and contains >=1 record per subject

## Returns `summary_df`, with one record per subject and columns:
## - new_id: subject + rep ID (need for merging with summarized mental status
##           values calculated using various strategies)
## - del_actual: observed duration of delirium with complete data
## - ltci: simulated outcome variable | actual duration of delirium and bdel

## intltci: default = E(ltci) for someone with no delirium in motivating example

library(dplyr)

simulate_ltci <- function(df, bdel, intltci = 79.7103){
  npts <- length(unique(df$new_id))
  
  ## 1. Simulate random error ~ N(0, 1)
  epsilon <- rnorm(n = npts)

  ## 2. Calculate actual days delirious, then linear predictor (= actual score)
  summary_df <- df %>%
    group_by(new_id) %>%
    summarise(
      del_actual = sum(status == "Delirious")
    ) %>%
    ungroup()

  summary_df$ltci <- intltci + bdel * summary_df$del_actual + epsilon
  
  return(summary_df)

}