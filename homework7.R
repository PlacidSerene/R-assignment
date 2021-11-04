library(tidyr)
library(modelr)
library(scales)

load('college_scorecard.Rda')

df$npt <- df$npt4_priv
df$npt[is.na(df$npt)] <- df$npt4_pub[is.na(df$npt)]
df <- df[!is.na(df$npt),]
df <- df %>% select(-c(npt4_pub,npt4_priv))

df$actcm25[is.na(df$actcm25)] <- mean(df$actcm25, na.rm = TRUE)
df$actcm75[is.na(df$actcm75)] <- mean(df$actcm75, na.rm = TRUE)

load('census_track.Rda')
load('college_flips.Rda')

df <- df %>% inner_join(college, by='unitid')

census_tracks$track_flips <- paste0()