## Strategies for Analyzing Summary Variables in the Presence of Partially Missing Longitudinal Data

JSM 2018 talk, eventual paper. README will be updated as we go.

### Scripts

#### Data Preparation/Creation

- `seed_prep.R`: Deidentify/subset data from motivating clinical example to
extract minimum information required
- `simulate_data.R`: Create datasets used for simulations. Datasets saved in
`analysisdata/` (currently not on GitHub).
- `simulate_outcome.R`: Given datasets created in `simulate_data.R`, simulate
the outcome of interest given the actual exposure values and specified
relationships. Datasets saved in `analysisdata/` (currently not on GitHub).

#### Simulation Functions

- `introduce_missing.R`: Functions which introduce missingness to the mental
status variable (`status`) in a dataset, given a type of missingness (MCAR vs
MAR/MNAR) and, if MAR/MNAR, a specified relationship between missingness and
daily severity of illness.
- `summary_strategies.R`: Functions which summarize the exposure in various ways,
preparing summarized datasets for model fitting. Strategies:
    1. "Ignore": Ignore missing records; total exposure = sum(all observed exposure)
    1. "Worst": Assume all missing records have the exposure; total exposure = sum(all observed + all missing exposure)
    1. "Delete": Any subject with any missing records gets a value of NA, which will be accounted for using multiple imputation at the time of modeling.
    1. "Impute": Impute daily exposure status using daily covariate, then summarize exposure for each imputation. Use those imputed summary datasets in model-based imputation.
- `fit_models.R`: Two functions to fit models which 1) do not and 2) require `mice::mids()` objects.

Overview of workflow is [here]((https://htmlpreview.github.io/?https://github.com/jenniferthompson/MissSumVars/blob/master/workflow.html)).