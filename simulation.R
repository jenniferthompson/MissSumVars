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

# ## -- Run miss_sum_fit() on "all" datasets -------------------------------------
# sim_results <- future_pmap_dfr(
#   .l = list(
#     df_long = simdata_list,
#     df_out_org = outcomedata_list,
#     seed_set = sim_seeds,
#     df_id = 1:length(simdata_list)
#   ),
#   miss_sum_fit, num_imp = 20,
#   .progress = TRUE
# )
# 
# saveRDS(sim_results, file = "results/sim_results.rds")

## -- Run miss_sum_fit() in chunks ---------------------------------------------
sim_results_250 <- future_pmap_dfr(
  .l = list(
    df_long = simdata_list[1:250],
    df_out_org = outcomedata_list[1:250],
    seed_set = sim_seeds[1:250],
    df_id = 1:250
  ),
  miss_sum_fit, num_imp = 20,
  .progress = TRUE
)

saveRDS(sim_results_250, file = "results/sim_results_250.rds")
BRRR::skrrrahh("biggie") ## optional, but fun

sim_results_500 <- future_pmap_dfr(
  .l = list(
    df_long = simdata_list[251:500],
    df_out_org = outcomedata_list[251:500],
    seed_set = sim_seeds[251:500],
    df_id = 251:500
  ),
  miss_sum_fit, num_imp = 20,
  .progress = TRUE
)

saveRDS(sim_results_500, file = "results/sim_results_500.rds")
BRRR::skrrrahh("biggie") ## optional, but fun

sim_results_750 <- future_pmap_dfr(
  .l = list(
    df_long = simdata_list[501:750],
    df_out_org = outcomedata_list[501:750],
    seed_set = sim_seeds[501:750],
    df_id = 501:750
  ),
  miss_sum_fit, num_imp = 20,
  .progress = TRUE
)

saveRDS(sim_results_750, file = "results/sim_results_750.rds")
BRRR::skrrrahh("biggie") ## optional, but fun

sim_results_1000 <- future_pmap_dfr(
  .l = list(
    df_long = simdata_list[751:1000],
    df_out_org = outcomedata_list[751:1000],
    seed_set = sim_seeds[751:1000],
    df_id = 751:1000
  ),
  miss_sum_fit, num_imp = 20,
  .progress = TRUE
)

saveRDS(sim_results_1000, file = "results/sim_results_1000.rds")
BRRR::skrrrahh("biggie") ## optional, but fun

## purrr vs furrr info
## With 10 datasets and three betas (0, -0.25, -0.5):
## furrr = 294.651sec
## purrr = 677.094sec
## !!!!!
## furrr with five betas: 324.07
