library(rscorecard)
library(rstudioapi)

# Key registration https://collegescorecard.ed.gov/data/documentation/
my_key <- askForPassword()
sc_key(my_key)

# Population predominantly bachelor's-degree granting colleges (preddeg==3)
df <- sc_init() %>% 
  sc_filter(preddeg==3) %>%
  sc_select(unitid, instnm, stabbr, city, actcm25, actcm75, ugds, control, distanceonly,
            md_earn_wne_p6, md_earn_wne_p8, md_earn_wne_p10,
            npt4_pub,npt4_priv, 
            omawdp8_ftft, 
            latitude,longitude) %>%
  sc_year('latest') %>% 
  sc_get()

# Exclusions 
# (1) Not online only (DISTANCEONLY == 0)
# (2) College in US state (exclude DC and US territories)

excluded_strabbr <- unique(df$stabbr[!df$stabbr %in% state.abb])
df <- df[df$stabbr %in% state.abb,]
df <- df[df$distanceonly == 0,]
save(df, file = 'C:/Users/EM020213/Downloads/college_scorecard.Rda')
