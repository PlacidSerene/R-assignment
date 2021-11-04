library(tidyverse)
# Since it's  time series, we need to convert it into a dataframe
df <- data.frame(as.matrix(Seatbelts), date=time(Seatbelts))
df$year <- trunc(df$date)
df$month <- (df$date - df$year) * 12 + 1
# Create our graph for driverskilled

plot1 <- ggplot(data=df, mapping = aes(x=DriversKilled)) +geom_histogram(bins = 20) #distribution
plot2 <- ggplot(data=df, mapping = aes(x=date, y=DriversKilled)) + geom_line()


# filter our data
df_short = filter(df, date>=1983)
plot2_short <- ggplot(data=df_short, mapping = aes(x=date, y=DriversKilled)) + geom_line()

# descriptive statistic
#mean
mean_death = mean(df$DriversKilled)
#max
max_death = max(df$DriversKilled)
max_df_date = select(filter(df, DriversKilled==max_death), year, month)
#min
min_death = min(df$DriversKilled)
min_df_date = select(filter(df, DriversKilled==min_death), year, month)
#median
median_death = median(df$DriversKilled)


mean_death_before <- df %>% filter(year<1983) %>% summarise(average_death_before=mean(DriversKilled))
mean_death_after <- df %>% filter(!(year<1983)) %>% summarise(average_death_after=mean(DriversKilled))
