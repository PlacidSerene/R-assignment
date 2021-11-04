## recommended packages
library(dplyr)
library(ggplot2)
library(gridExtra)

## load data
load('college_scorecard.Rda')


#Clean up college scorecard data
#Create a new variable, npt, that combines npt4_priv and npt4_pub
#Filter df where npt is not null, !is.na().
#Add a factor variable for control with labels 1=public, 2=private, 3=for profit

df$npt <- df$npt4_priv
df$npt[is.na(df$npt)] <- df$npt4_pub[is.na(df$npt)]
df <- df %>% filter(!is.na(npt))
