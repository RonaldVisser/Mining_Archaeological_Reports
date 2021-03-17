library(readxl)
library(dplyr)
library(ggplot2)
library(ggalt)
library(showtext)
library(svglite)
library(ggpubr)

options(scipen = 999)

font_add("RijksoverheidSerif-Regular_2", "RijksoverheidSerif-Regular_2.otf")
showtext_auto()
trace(grDevices::svg, exit = quote({showtext::showtext_begin()}), print = FALSE)


# read data
date_keyword_stad <- read_excel("data/qry_thema_keyword_datering_steden.xlsx")
# directory for export
dir.create("export", showWarnings = FALSE)
dir.create("export/zeven_steden", showWarnings = FALSE)


date_keyword_stad <- date_keyword_stad %>% 
  mutate(per_length = (eind - begin))

date_keyword_stad <- date_keyword_stad %>% 
  mutate(per_mid = (begin + per_length/2), per_sd = (0.34*(per_length/2)))

set.seed(2000)
years <- mapply(seq,date_keyword_stad$begin, date_keyword_stad$eind)
periods <- lengths(years)
prob_period <- unlist(mapply(dnorm, years, date_keyword_stad$per_mid, date_keyword_stad$per_sd))
years <- unlist(years)
keyword_rep <- rep(date_keyword_stad$keyword, periods)
stad_rep <- rep(date_keyword_stad$Stad, periods)
keyword_period_prob <- cbind.data.frame(keyword_rep, stad_rep, years, prob_period)
colnames(keyword_period_prob) <- c("keyword","stad", "year", "probability")
rm(years, prob_period, keyword_rep, stad_rep)

# summarized probability of all data
period_prob_sum <- keyword_period_prob %>% group_by(year) %>% summarise(totaal = sum(as.numeric(probability)))
# plot summarized probabilities 
prob_sum <- ggplot(period_prob_sum,aes(x=year, y=totaal)) + geom_path() +
  xlim(500,2000) + theme_grey(base_family = "RijksoverheidSerif-Regular_2") +
  ylab("summarized probability density")
ggsave("export/zeven_steden/datering_probability_steden_sum.svg", prob_sum, dpi= 300, width=12, height = 8)

date_prob <- ggplot(keyword_period_prob,aes(x=year, y=probability)) + geom_path() + 
  scale_x_continuous(breaks = seq(500, 2000, by = 250), limits = c(500,2000)) + 
  facet_wrap(~stad, ncol = 2) + theme_grey(base_family = "RijksoverheidSerif-Regular_2")
ggsave("export/zeven_steden/datering_probability_stad.svg", date_prob, dpi= 300, width=12, height = 8)

period_prob_stad_sum <- keyword_period_prob %>% group_by(year,stad) %>% summarise(totaal = sum(as.numeric(probability)))

date_prob_sum <- ggplot(period_prob_stad_sum,aes(x=year, y=totaal)) + geom_path() + 
  scale_x_continuous(breaks = seq(500, 2000, by = 250), limits = c(500,2000)) + 
  facet_wrap(~stad, ncol = 2) + theme_grey(base_family = "RijksoverheidSerif-Regular_2") +
  ylab("summarized probability density")
ggsave("export/zeven_steden/datering_probability_stad_sum.svg", date_prob_sum, dpi= 300, width=12, height = 8)

hist_town <- ggplot(date_keyword_stad, aes(x=Stad)) + geom_bar() + xlab("") +
  theme_grey(base_family = "RijksoverheidSerif-Regular_2")
ggsave("export/zeven_steden/steden_bar.svg", hist_town, dpi= 300, width=12, height = 8) 

date_prob7 <- ggplot(keyword_period_prob,aes(x=year, y=probability)) + geom_path() + 
  scale_x_continuous(breaks = seq(500, 2000, by = 500), limits = c(500,2000)) + 
  facet_wrap(~stad, ncol = 7) + theme_grey(base_family = "RijksoverheidSerif-Regular_2") + 
  theme(axis.text.x = element_text(angle = 90), axis.title=element_text(size=8)) +
  ylab("Probability density")
date_prob_sum7 <- ggplot(period_prob_stad_sum,aes(x=year, y=totaal)) + geom_path() + 
  scale_x_continuous(breaks = seq(500, 2000, by = 500), limits = c(500,2000)) + 
  facet_wrap(~stad, ncol = 7) + theme_grey(base_family = "RijksoverheidSerif-Regular_2") + 
  theme(axis.text.x = element_text(angle = 90), axis.title=element_text(size=8)) + 
  ylab("summarized probability density")

ggarrange(ggarrange(prob_sum + theme(axis.text.x = element_text(angle = 90)), hist_town), 
          date_prob7, date_prob_sum7, ncol = 1, heights = c(2,1,1))
ggsave("export/zeven_steden/seven_towns.svg", dpi= 300, width=14, height = 8)
ggsave("export/zeven_steden/seven_towns.png", dpi= 600, width=14, height = 8)


