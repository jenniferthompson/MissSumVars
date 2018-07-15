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
#   miss_sum_fit, num_imp = 3,
#   .progress = TRUE
# )

# saveRDS(sim_results, file = "results/sim_results.rds")

## -- Run miss_sum_fit() in chunks ---------------------------------------------
run_sim_results <- function(start_id, end_id){
  sim_chunk <- future_pmap_dfr(
    .l = list(
      df_long = simdata_list[start_id:end_id],
      df_out_org = outcomedata_list[start_id:end_id],
      seed_set = sim_seeds[start_id:end_id],
      df_id = start_id:end_id
    ),
    miss_sum_fit, num_imp = 20,
    .progress = TRUE
  )
  saveRDS(
    sim_chunk,
    file = sprintf("results/sim_results_%s_%s.rds", start_id, end_id)
  )
  BRRR::skrrrahh(sample(c("biggie", "khaled"), size = 1)) ## optional, but fun
}

# ## Test function
# walk2(
#   .x = c(1, 3),
#   .y = c(2, 4),
#   .f = run_sim_results
# )

walk2(
  .x = seq(1, 901, 100),
  .y = seq(100, 1000, 100),
  .f = run_sim_results
)

## purrr vs furrr info
## With 10 datasets and three betas (0, -0.25, -0.5):
## furrr = 294.651sec
## purrr = 677.094sec
## !!!!!
## furrr with five betas: 324.07
