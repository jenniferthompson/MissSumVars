################################################################################
## Simulate data using "seed" data from motivating clinical example
################################################################################

## -- CURRENTLY JUST A SKETCH WITH ONE DATASET ---------------------------------
## Eventually this will be a function with arguments for various parameters

library(purrr)
library(furrr) ## for parallel processing of iteration with purrr
library(dplyr)
library(tidyr)
library(tictoc)
  ## Just for comparing furrr vs purrr timings; can remove tic(), toc() calls

## Read in seed data, get vector of unique IDs
seed_df <- readRDS("rawdata/seeddf.rds")
all_ids <- unique(seed_df$id)

## -- Function to create a single simulated dataset ----------------------------
create_sim_df <- function(seed_set, npts, seed_df, id_list){
  ## Set seed for reproducibility
  set.seed(seed_set)

  ## Sample list of IDs, length npts, from original dataset  
  sample_ids <- sample(id_list, size = npts, replace = TRUE)
  
  sample_df <- map_dfr(
    sample_ids, ~ subset(seed_df, id == .),
    .id = "rep"
  ) %>%
    ## Create *new* ID variable that differentiates original IDs included >1 time
    unite(new_id, id, rep, remove = FALSE)
  
  return(sample_df)
}

## -- Map over vector of seeds -> list of 1000 dfs with 200 patients each ------
## For furrr functions, set plan (see futures package, furrr README)
plan(multiprocess)

tic()
simdata_list <- future_map(
  8675309:(8675309 + 999),
  create_sim_df,
  npts = 200, seed_df = seed_df, id_list = all_ids
)
toc()

# ## For comparison - this is my first time using furrr
# tic()
# simdata_list_purrr <- map(
#   8675309:(8675309 + 999),
#   create_sim_df,
#   npts = 200, seed_df = seed_df, id_list = all_ids
# )
# toc()
# 
# ## Make sure these get the same results:
# sum(map_lgl(1:1000, ~ all(simdata_list[[.]] == simdata_list_purrr[[.]])))

saveRDS(simdata_list, file = "analysisdata/simdata.rds")
