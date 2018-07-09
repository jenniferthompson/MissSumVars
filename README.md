## Strategies for Analyzing Summary Variables in the Presence of Partially Missing Longitudinal Data

JSM 2018 talk, eventual paper. README will be updated as we go.

Scripts:

- `simulate_data.R`: Create datasets used for simulations. Datasets saved in
`analysisdata/` (currently not on GitHub).
- `introduce_missing.R`: Functions which introduce missingness to the mental
status variable (`status`) in a dataset, given a type of missingness (MCAR vs
MAR/MNAR) and, if MAR/MNAR, a specified relationship between missingness and
daily severity of illness.
- `simulate_outcome.R`: Function which simulates outcome (score measuring
global cognitive ability), given a longitudinal dataset and a specified
relationship between total days of delirium and cognition.