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

# directory for export
dir.create("export", showWarnings = FALSE)
dir.create("export/gewesten", showWarnings = FALSE)


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
gewest_rep <- rep(date_keyword_gewest$gewest, periods)
keyword_period_prob <- cbind.data.frame(keyword_rep, gewest_rep, years, prob_period)
colnames(keyword_period_prob) <- c("keyword","gewest", "year", "probability")
rm(years, prob_period, keyword_rep, gewest_rep)

# summarized probability of all data
period_prob_sum <- keyword_period_prob %>% group_by(year) %>% summarise(totaal = sum(as.numeric(probability)))
# plot summarized probabilities 
ggplot(period_prob_sum,aes(x=year, y=totaal)) + geom_path() +xlim(500,2000)
ggsave("export/gewesten/datering_probability_gewesten_sum.png", dpi= 300, width=12, height = 8)

ggplot(keyword_period_prob,aes(x=year, y=probability)) + geom_path() + 
  scale_x_continuous(breaks = seq(500, 2000, by = 250), limits = c(500,2000)) + 
  facet_wrap(~gewest, ncol = 4)
ggsave("export/gewesten/datering_probability_gewesten.png", dpi=300, width=12, height = 8)

period_prob_gewest_sum <- keyword_period_prob %>% group_by(year,gewest) %>% summarise(totaal = sum(as.numeric(probability)))
ggplot(period_prob_gewest_sum,aes(x=year, y=totaal)) + geom_path() + 
  scale_x_continuous(breaks = seq(500, 2000, by = 250), limits = c(500,2000)) + 
  facet_wrap(~gewest, ncol = 4)
ggsave("export/gewesten/datering_probability_gewest_sum.png", dpi= 300, width=12, height = 8)

ggplot(date_keyword_gewest, aes(x=gewest)) + geom_bar() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5))
ggsave("export/gewesten/gewest_bar.png", dpi= 300, width=12, height = 8)

