################################################################################
## Prepare only data needed from example study
## Includes in-hospital data (SOI, mental status) for only patients who survived
##  to long-term followup
################################################################################

library(dplyr)

## -- Dataset #1: Prospective cohort -------------------------------------------

## Data was downloaded from safe storage spot, will be deleted once we're done
load("../Downloads/seeddata.Rdata")
## Remove variable labels; they are causing pain
## Thanks, StackOverflow https://stackoverflow.com/questions/2394902/remove-variable-labels-attached-with-foreign-hmisc-spss-import-functions
clear.labels <- function(x) {
  if(is.list(x)) {
    for(i in 1 : length(x)) class(x[[i]]) <- setdiff(class(x[[i]]), 'labelled') 
    for(i in 1 : length(x)) attr(x[[i]],"label") <- NULL
  }
  else {
    class(x) <- setdiff(class(x), "labelled")
    attr(x, "label") <- NULL
  }
  return(x)
}
brain.oneobs <- clear.labels(brain.oneobs)
brain.daily <- clear.labels(brain.daily)
brain.fu <- clear.labels(brain.fu)

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

## Save final file used for simulations
# saveRDS(save_df, file = "rawdata/seeddf.rds")

## -- Save "real" data for "real" analyses -------------------------------------
## Keep only patients with long-term outcomes; keep only in-hospital days
## (status is *not* discharged, deceased, withdrawn)
study1_daily <- subset(
  brain.daily,
  id %in% ids_lt &
    (is.na(status.today) |
       status.today %in% c("Normal", "Delirious", "Comatose")),
  select = c(id, study.day, daily.sofa.mod.locf, status.today)
)
names(study1_daily) <- c("id", "study_day", "sofa_mod", "status")
study1_daily$id <- match(study1_daily$id, ids_lt)

study1_fu <- subset(
  brain.fu,
  fu.period == "12 Month" & !is.na(rbans.global.score),
  select = c(id, rbans.global.score)
)
study1_fu$id <- match(study1_fu$id, ids_lt)

study1_df <- merge(study1_daily, study1_fu, by = "id", all.x = TRUE)
names(study1_df)[ncol(study1_df)] <- "outcome"
study1_df$status <- as.character(study1_df$status)

saveRDS(study1_df, file = "rawdata/study1.rds")

## -- Dataset #2: EMR data -----------------------------------------------------

## Data was downloaded from safe storage spot, will be deleted once we're done
load("../Downloads/seeddata2.Rdata")

tbi.oneobs <- clear.labels(tbi.oneobs)
tbi.daily <- clear.labels(tbi.daily)

## Anonymize IDs
mrns <- sort(unique(tbi.oneobs$mrn))
ids <- 1:length(mrns)

tbi.oneobs$id <- ids[match(tbi.oneobs$mrn, mrns)]
tbi.daily$id <- ids[match(tbi.daily$mrn, mrns)]

study2_daily <- subset(
  tbi.daily, select = c(id, study.day, mental.status, sofa.mod.nanormal.imp)
)
names(study2_daily) <- c("id", "study_day", "status", "sofa_mod")

## Only keep patients with a FIM score (hospital survivors)
## No need to restrict statuses; this dataset only has in-hospital days
has_fim <- subset(tbi.oneobs, !is.na(fim.total))$id

study2_daily <- subset(study2_daily, id %in% has_fim)

study2_oneobs <- subset(tbi.oneobs, !is.na(fim.total), select = c(id, fim.total))
names(study2_oneobs) <- c("id", "outcome")

study2_df <- merge(study2_daily, study2_oneobs, by = "id")
study2_df$status <- as.character(study2_df$status)

saveRDS(study2_df, file = "rawdata/study2.rds")
