################################################################################
## Simulation script: Introduce missingness, summarize exposure, and fit models
##  for each simulated dataset
################################################################################

## -- Prep: Load libraries, source functions, read in simulated datasets -------
library(purrr)
library(furrr)
library(dplyr)
library(mice)

plan(multiprocess)

source("introduce_missing.R")
source("summary_strategies.R")
source("fit_models.R")
source("miss_sum_fit.R")

simdata_list <- readRDS("analysisdata/simdata.rds")
outcomedata_list <- readRDS("analysisdata/outcomedata.rds")
sim_seeds <- 8675309:(8675309 + (length(simdata_list) - 1))

take_these <- 1:1000 ## could change for testing purposes

if(max(take_these) < length(simdata_list)){
  simdata_list <- simdata_list[take_these]
  outcomedata_list <- outcomedata_list[take_these]
  sim_seeds <- sim_seeds[take_these]
}

## -- Run miss_sum_fit() on "all" datasets -------------------------------------
sim_results <- future_pmap_dfr(
  .l = list(
    df_long = simdata_list,
    df_out_org = outcomedata_list,
    seed_set = sim_seeds,
    df_id = 1:length(simdata_list)
  ),
  miss_sum_fit, num_imp = 20,
  .progress = TRUE
)

saveRDS(sim_results, file = "results/sim_results.rds")

## -- Scratch code -------------------------------------------------------------
# tmp <- miss_sum_fit(
#   df_long = simdata_list[[1]],
#   df_out_org = outcomedata_list[[1]],
#   seed_set = sim_seeds[1],
#   num_imp = 3,
#   df_id = 1
# )
# 
# dfs_missing <- intro_missing(df = simdata_list[[1]], seed_set = 5)
# dfs_delete <- map(dfs_missing, poss_summarize_delete, seed_set = 10)
# dfs_impute <- map(dfs_missing, summarize_impute, seed_set = 15)
