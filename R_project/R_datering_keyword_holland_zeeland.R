library(readxl)
library(dplyr)
library(ggplot2)
library(ggalt)

options(scipen = 999)

# read data
date_keyword_stad <- read_excel("data/qry_thema_keyword_datering_provincie.xlsx")
steden_gewesten <- read_excel("data/OverzichtAtlasGewesten.xlsx")
date_keyword_gewest <- left_join(date_keyword_stad, steden_gewesten, by = "Stad")
date_keyword_gewest <- select (date_keyword_gewest,-provincie.x)
date_keyword_gewest <- rename(date_keyword_gewest, provincie = provincie.y)
date_keyword_gewest_1 <- date_keyword_gewest %>%
  filter(gewest == 'Holland' | gewest == 'Zeeland') %>%
  mutate(Holland_Zeeland = 'Holland/Zeeland')
date_keyword_gewest_2 <- date_keyword_gewest %>%
  filter(gewest != 'Holland' & gewest != 'Zeeland') %>%
  mutate(Holland_Zeeland = 'Other')
date_keyword_gewest <- bind_rows(date_keyword_gewest_1,date_keyword_gewest_2)
rm(date_keyword_gewest_1,date_keyword_gewest_2)

# directory for export
dir.create("export", showWarnings = FALSE)
dir.create("export/Holland_Zeeland", showWarnings = FALSE)


date_keyword_gewest <- date_keyword_gewest %>% 
  mutate(per_length = (eind - begin))

date_keyword_gewest <- date_keyword_gewest %>% 
  mutate(per_mid = (begin + per_length/2), per_sd = (0.34*(per_length/2)))

set.seed(2000)
years <- mapply(seq,date_keyword_gewest$begin, date_keyword_gewest$eind)
periods <- lengths(years)
prob_period <- unlist(mapply(dnorm, years, date_keyword_gewest$per_mid, date_keyword_gewest$per_sd))
years <- unlist(years)
keyword_rep <- rep(date_keyword_gewest$keyword, periods)
gewest_rep <- rep(date_keyword_gewest$Holland_Zeeland, periods)
keyword_period_prob <- cbind.data.frame(keyword_rep, gewest_rep, years, prob_period)
colnames(keyword_period_prob) <- c("keyword","Holland_Zeeland", "year", "probability")
rm(years, prob_period, keyword_rep, gewest_rep)

# summarized probability of all data
period_prob_sum <- keyword_period_prob %>% group_by(year) %>% summarise(totaal = sum(as.numeric(probability)))
# plot summarized probabilities 
ggplot(period_prob_sum,aes(x=year, y=totaal)) + geom_path() +xlim(500,2000)
ggsave("export/Holland_Zeeland/datering_probability_Holland_Zeeland_sum.png", dpi= 300, width=12, height = 8)

ggplot(keyword_period_prob,aes(x=year, y=probability)) + geom_path() + 
  scale_x_continuous(breaks = seq(500, 2000, by = 250), limits = c(500,2000)) + 
  facet_wrap(~Holland_Zeeland, ncol = 1)
ggsave("export/Holland_Zeeland/datering_probability_Holland_Zeeland.png", dpi=300, width=12, height = 12)

period_prob_gewest_sum <- keyword_period_prob %>% group_by(year,Holland_Zeeland) %>% summarise(totaal = sum(as.numeric(probability)))
ggplot(period_prob_gewest_sum,aes(x=year, y=totaal)) + geom_path() + 
  scale_x_continuous(breaks = seq(500, 2000, by = 250), limits = c(500,2000)) + 
  facet_wrap(~Holland_Zeeland, ncol = 1)
ggsave("export/Holland_Zeeland/datering_probability_Holland_Zeeland_sum.png", dpi= 300, width=12, height = 12)

ggplot(date_keyword_gewest, aes(x=Holland_Zeeland)) + geom_bar() 
ggsave("export/Holland_Zeeland/Holland_Zeeland_bar.png", dpi= 300, width=12, height = 8)

