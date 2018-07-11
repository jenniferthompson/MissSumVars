################################################################################
## Function to simulate cognitive scores based on *actual* days delirious in
## a given data.frame, given a specified relationship between delirium/RBANS
################################################################################

## Assumes `df` has columns `new_id` (subject + rep ID) and `status` (which
## takes values Normal, Delirious, Comatose) and contains >=1 record per subject

## Returns `outcome_df`, with one record per subject and columns:
## - new_id: subject + rep ID (need for merging with summarized mental status
##           values calculated using various strategies)
## - del_actual: observed duration of delirium with complete data
## - cogscore: simulated outcome variable | actual duration of delirium and bdel

## intcs: default = E(cogscore) for pt with no delirium in motivating example

library(dplyr)

simulate_cogscore <- function(
  df, ## Starting longitudinal data.frame
  bdel, ## average change in outcome for a one-day increase in delirium
  intcs = 79.7103 ## mean outcome for patients with no delirium
    ## default value comes from motivating example
){
  npts <- length(unique(df$new_id))
  
  ## 1. Simulate random error ~ N(0, 1)
  epsilon <- rnorm(n = npts)

  ## 2. Calculate actual days delirious, then linear predictor (= actual score)
  outcome_df <- df %>%
    group_by(new_id) %>%
    summarise(
      del_actual = sum(status == "Delirious")
    ) %>%
    ungroup()

  outcome_df$cogscore <- intcs + bdel * outcome_df$del_actual + epsilon
  
  return(outcome_df)

}