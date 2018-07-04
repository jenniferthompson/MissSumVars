################################################################################
## Prepare only data needed from example study
## Includes in-hospital data (SOI, mental status) for only patients who survived
##  to long-term followup
################################################################################

## Data was downloaded from safe storage spot, will be deleted once we're done
load("../Downloads/seeddata.Rdata")

## -- Which patients have 12m followup? ----------------------------------------
## These patients are likely different than the ones who don't survive/aren't
##  assessed at 12m and don't reflect the population we're interested in
ids_lt <-
  subset(brain.fu, fu.period == "12 Month" & !is.na(rbans.global.score))$id

## -- Save daily data on hospital days for these patients ----------------------
save_df <- subset(
  brain.daily,
  id %in% ids_lt & status.today.imp %in% c("Normal", "Delirious", "Comatose"),
  select = c(id, study.day, daily.sofa.mod.locf, status.today.imp)
)

## I usually use underscores now; shorten names
names(save_df) <- c("id", "study_day", "sofa_mod", "status")

## Anonymize IDs
save_df$id <- match(save_df$id, ids_lt)

## Save final file
saveRDS(save_df, file = "rawdata/seeddf.rds")