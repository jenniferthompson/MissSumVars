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

## Force status to be a character
save_df$status <- as.character(save_df$status)

## Save final file
saveRDS(save_df, file = "rawdata/seeddf.rds")

## Calculate intercept for simulations
fu_df <- subset(
  brain.fu,
  id %in% ids_lt & fu.period == "12 Month",
  select = c(id, rbans.global.score)
)

oneobs_df <- subset(
  brain.oneobs,
  id %in% ids_lt,
  select = c(id, del.s.imp)
)

int_df <- merge(oneobs_df, fu_df, by = "id")
int_mod <- lm(rbans.global.score ~ del.s.imp, data = int_df)
