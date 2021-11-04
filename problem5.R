library(purrr) # recommend keep()/discharge() and map_dbl()
library(dplyr)
library(tidyr)
library(ggplot2)
library(knitr)
library(scales)

load('college_scorecard.Rda')
df_new <- df
df_new$npt <- df_new$npt4_priv
df_new$npt[is.na(df_new$npt)] <- df_new$npt4_pub[is.na(df_new$npt)]
df_new <- df_new %>% filter(!is.na(df_new$npt))
df_new$control <- factor(df_new$control,level = 1:3,labels = c('public','private','for_profit'))
df_new$public <- as.numeric(df_new$control=='public')
df_new <- df_new %>% select(-distanceonly)

# Questions
#1. 
colleges_excluded <- nrow(df) - nrow(df_new)
colleges_remaining <- percent(nrow(df_new)/nrow(df))

#2. 
df_num <- df_new %>% keep(is.numeric) %>% pivot_longer(names_to='key', values_to ='value', -npt)

plot1 <- ggplot(df_num, aes(x=value, y=npt)) + geom_point(alpha=.2,size=1) + facet_wrap(~key, scales='free') 
#3.
coef <- df_num %>% group_by(key) %>% summary(coefficient = cor(npt, key, use='complete.obs'))
