## Strategies for Analyzing Summary Variables in the Presence of Partially Missing Longitudinal Data

First presented at JSM 2018, Vancouver. Code for creating that presentation, including all visuals, is in the `JSM2018/` directory. Slides (HTML) are at [bit.ly/jlt-jsm2018](https://jenthompson.me/slides/jsm2018/jsm2018#1); a PDF version is [here](https://github.com/jenniferthompson/MissSumVars/blob/master/JSM2018/jsm2018_slides.pdf).

### Scripts

#### Data Preparation/Creation

- `seed_prep.R`: Deidentify/subset data from motivating clinical examples
- `simulate_data.R`: Create datasets used for simulations. Datasets saved in
`analysisdata/` (not on GitHub).
- `simulate_outcome.R`: Given datasets created in `simulate_data.R`, simulate
the outcome of interest given the actual exposure values and specified
relationships. Datasets saved in `analysisdata/` (not on GitHub).

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
- `miss_sum_fit.R`: For a given simulated dataset, introduce all kinds of missingness; summarize the exposure in four ways; fit and extract needed info from models
- `simulation.R`: Run entire simulation on 1000 datasets, in batches of 100

`realworld.R` applies these strategies to two deidentified actual datasets, saving the results to be plotted.

Sketch of workflow for a single dataset is [here](https://htmlpreview.github.io/?https://github.com/jenniferthompson/MissSumVars/blob/master/workflow.html).