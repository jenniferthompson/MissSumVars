################################################################################
## Function to simulate cognitive scores based on *actual* days delirious in
## a given data.frame, given a specified relationship between delirium/RBANS
################################################################################

library(purrr)
library(dplyr)

## Assumes `df` has columns `new_id` (subject + rep ID) and `status` (which
## takes values Normal, Delirious, Comatose) and contains >=1 record per subject

## Returns `outcome_df`, with one record per subject and columns:
## - new_id: subject + rep ID (need for merging with summarized mental status
##           values calculated using various strategies)
## - del_actual: observed duration of delirium with complete data
## - cogscore: simulated outcome variable | actual duration of delirium and bdel

## intcs: default = E(cogscore) for pt with no delirium in motivating example

simulate_cogscore <- function(
  df, ## Starting longitudinal data.frame
  bdel, ## average change in outcome for a one-day increase in delirium
  intcs = 120 ## max score possible; should yield positive values in simulation
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
  outcome_df$true_beta <- bdel
  
  return(outcome_df)

}

## -- Simulate all datasets; save in analysisdata/ -----------------------------
## For each dataset created in `simulate_data.R`, want corresponding dfs with
## betas for delirium vs scores of -1, -3, -5 (~ one-, three-, five-point drops
## in outcome for each day of delirium)

simdata_list <- readRDS("analysisdata/simdata.rds")

## Get all combinations
outcome_args <- cross2(.x = simdata_list, .y = c(-1, -3, -5)) %>%
  map(set_names, c("df", "bdel"))

## furrr *not* faster in this case (17.225sec vs 16.615)
simoutcomes_list <- map(
  outcome_args,
  ~ simulate_cogscore(df = pluck(., "df"), bdel = pluck(., "bdel"))
)

saveRDS(simoutcomes_list, file = "analysisdata/outcomedata.rds")