library(readxl)
library(dplyr)
library(ggplot2)
library(ggalt)
#library(RColorBrewer)
#library(pastecs)

options(scipen = 999)

# read data
date_keyword <- read_excel("data/qry_thema_keyword_datering.xlsx")
# directory for export
dir.create("export", showWarnings = FALSE)

date_keyword <- date_keyword %>% 
  mutate(per_length = (eind - begin))

date_keyword <- date_keyword %>% 
  mutate(per_mid = (begin + per_length/2), per_sd = (0.34*(per_length/2)))



set.seed(2000)
#years <- seq(date_keyword$begin, date_keyword$eind)
years <- mapply(seq,date_keyword$begin, date_keyword$eind)
periods <- lengths(years)
#prob_period <- dnorm(years,date_keyword$per_mid[1], date_keyword$per_sd[1])
prob_period <- unlist(mapply(dnorm, years, date_keyword$per_mid, date_keyword$per_sd))
years <- unlist(years)
keyword_rep <- rep(date_keyword$keyword, periods)
thema_rep <- rep(date_keyword$thema, periods)
keyword_period_prob <- cbind.data.frame(keyword_rep, thema_rep, years, prob_period)
colnames(keyword_period_prob) <- c("keyword","thema", "year", "probability")
rm(years, prob_period, keyword_rep, thema_rep)

# summarized probability of all data
period_prob_sum <- keyword_period_prob %>% group_by(year) %>% summarise(totaal = sum(as.numeric(probability)))
# plot summarized probabilities 
ggplot(period_prob_sum,aes(x=year, y=totaal)) + geom_path() +xlim(500,2000)
ggsave("export/datering_probability_sum.png", dpi= 300, width=12, height = 8)

# summarized probability of theme's
thema_period_prob_sum <- keyword_period_prob %>% group_by(thema,year) %>% summarise(totaal = sum(as.numeric(probability)))
# summarized probability of keywords (by theme)
keyword_period_prob_sum <- keyword_period_prob %>% group_by(thema,keyword,year) %>% summarise(totaal = sum(as.numeric(probability)))

#keyword_period_prob[1:2150,] %>%
#  ggplot(aes(x=year, y=probability)) + geom_path()
#keyword_period_prob[1:2150,] %>% group_by(year) %>% summarise(probability = sum(as.numeric(probability))) %>%
#  ggplot(aes(x=year, y=probability)) + geom_path()

# example to explain method
selection_example <- keyword_period_prob[1:2150,] %>% filter (thema=="tuinbouw")
ggplot(selection_example, aes(x=year, y=probability, color = keyword)) + geom_path() + ylim(0,0.006)
ggsave("export/example_datering_probability.png", dpi= 300, width=12, height = 8)
selection_example_sum <- keyword_period_prob[1:2150,] %>% filter (thema=="tuinbouw") %>% group_by(year) %>% summarise(probability = sum(as.numeric(probability)))
selection_example_sum <- cbind(rep("probability_sum",length(selection_example_sum)),rep("probability_sum",length(selection_example_sum)),selection_example_sum)
colnames(selection_example_sum) <- colnames(selection_example) <- c("keyword","thema", "year", "probability")
selection_example <- rbind(selection_example, selection_example_sum)
rm(selection_example_sum)
ggplot(selection_example, aes(x=year, y=probability, color = keyword)) + geom_path() + ylim(0,0.006)
ggsave("export/example_datering_probability_sum.png", dpi= 300, width=12, height = 8)

# totalen (x-limit 500-2000 na Chr.)
ggplot(keyword_period_prob,aes(x=year, y=probability)) + geom_path() + 
  scale_x_continuous(breaks = seq(500, 2000, by = 250), limits = c(500,2000)) + 
  facet_wrap(~thema, ncol = 2)
ggsave("export/thema_datering_probability.png", dpi= 300, width=12, height = 8)

ggplot(thema_period_prob_sum,aes(x=year, y=totaal)) + geom_path() + 
  scale_x_continuous(breaks = seq(500, 2000, by = 250), limits = c(500,2000)) + 
  facet_wrap(~thema, ncol = 2)
ggsave("export/thema_datering_probability_sum.png", dpi= 300, width=12, height = 8)


# akkerbouw
date_keyword_akkerbouw <- date_keyword %>% filter(thema == 'akkerbouw')
ggplot(date_keyword_akkerbouw, aes(y=keyword)) +
  geom_point(aes(x=per_mid, size=waardering), alpha=0.25) + scale_size_continuous(range = c(0.1, 2)) +
  geom_dumbbell(aes(x=begin, xend=eind), alpha=0.1, size_x =0.1, size_xend=0.1) +
  scale_x_continuous(breaks = seq(500, 2000, by = 250), limits = c(500,2000)) + 
  xlab("Datering")
ggsave("export/date_keyword_akkerbouw.png", dpi= 300, width=12, height = 8)

keyword_period_prob %>% filter(thema == "akkerbouw") %>%
  ggplot(aes(x=year, y=probability)) + geom_path() +
  scale_x_continuous(breaks = seq(500, 2000, by = 250), limits = c(500,2000)) + 
  facet_wrap(~keyword, ncol = 6)
ggsave("export/akkerbouw_datering_probability.png", dpi= 300, width=12, height = 12)

keyword_period_prob_sum %>% filter(thema == "akkerbouw") %>%
  ggplot(aes(x=year, y=totaal)) + geom_path() + 
  scale_x_continuous(breaks = seq(500, 2000, by = 250), limits = c(500,2000)) + 
  facet_wrap(~keyword, ncol = 6)
ggsave("export/akkerbouw_datering_probability_sum.png", dpi= 300, width=12, height = 12)


# algemeen
date_keyword_algemeen <- date_keyword %>% filter(thema == 'algemeen')
ggplot(date_keyword_algemeen, aes(y=keyword)) +
  geom_point(aes(x=per_mid, size=waardering), alpha=0.25) + scale_size_continuous(range = c(0.1, 2)) +
  geom_dumbbell(aes(x=begin, xend=eind), alpha=0.1, size_x =0.1, size_xend=0.1) +
  scale_x_continuous(breaks = seq(500, 2000, by = 250), limits = c(500,2000)) + 
  xlab("Datering")
ggsave("export/date_keyword_algemeen.png", dpi= 300, width=12, height = 8)

keyword_period_prob %>% filter(thema == "algemeen") %>%
  ggplot(aes(x=year, y=probability)) + geom_path() + 
  scale_x_continuous(breaks = seq(500, 2000, by = 250), limits = c(500,2000)) + 
  facet_wrap(~keyword, ncol = 6)
ggsave("export/algemeen_datering_probability.png", dpi= 300, width=12, height = 12)

keyword_period_prob_sum %>% filter(thema == "algemeen") %>%
  ggplot(aes(x=year, y=totaal)) + geom_path() + 
  scale_x_continuous(breaks = seq(500, 2000, by = 250), limits = c(500,2000)) + 
  facet_wrap(~keyword, ncol = 6)
ggsave("export/algemeen_datering_probability_sum.png", dpi= 300, width=12, height = 12)


# boomgaard
date_keyword_boomgaard <- date_keyword %>% filter(thema == 'boomgaard')
ggplot(date_keyword_boomgaard, aes(y=keyword)) +
  geom_point(aes(x=per_mid, size=waardering), alpha=0.25) + scale_size_continuous(range = c(0.1, 2)) +
  geom_dumbbell(aes(x=begin, xend=eind), alpha=0.1, size_x =0.1, size_xend=0.1) +
  scale_x_continuous(breaks = seq(500, 2000, by = 250), limits = c(500,2000)) + 
  xlab("Datering")
ggsave("export/date_keyword_boomgaard.png", dpi= 300, width=12, height = 8)

keyword_period_prob %>% filter(thema == "boomgaard") %>%
  ggplot(aes(x=year, y=probability)) + geom_path() + 
  scale_x_continuous(breaks = seq(500, 2000, by = 250), limits = c(500,2000)) + 
  facet_wrap(~keyword, ncol = 4)
ggsave("export/boomgaard_datering_probability.png", dpi= 300, width=12, height = 12)

keyword_period_prob_sum %>% filter(thema == "boomgaard") %>%
  ggplot(aes(x=year, y=totaal)) + geom_path() + 
  scale_x_continuous(breaks = seq(500, 2000, by = 250), limits = c(500,2000)) + 
  facet_wrap(~keyword, ncol = 4)
ggsave("export/boomgaard_datering_probability_sum.png", dpi= 300, width=12, height = 12)


# tuinbouw
date_keyword_tuinbouw <- date_keyword %>% filter(thema == 'tuinbouw')
ggplot(date_keyword_tuinbouw, aes(y=keyword)) + 
  geom_point(aes(x=per_mid, size=waardering), alpha=0.25) + scale_size_continuous(range = c(0.1, 2)) +
  geom_dumbbell(aes(x=begin, xend=eind), alpha=0.1, size_x =0.1, size_xend=0.1) +
  scale_x_continuous(breaks = seq(500, 2000, by = 250), limits = c(500,2000)) + 
  xlab("Datering")
ggsave("export/date_keyword_tuinbouw.png", dpi= 300, width=12, height = 8)

keyword_period_prob %>% filter(thema == "tuinbouw") %>%
  ggplot(aes(x=year, y=probability)) + geom_path() + 
  scale_x_continuous(breaks = seq(500, 2000, by = 250), limits = c(500,2000)) + 
  facet_wrap(~keyword, ncol = 6)
ggsave("export/tuinbouw_datering_probability.png", dpi= 300, width=12, height = 12)

keyword_period_prob_sum %>% filter(thema == "tuinbouw") %>%
  ggplot(aes(x=year, y=totaal)) + geom_path() +
  scale_x_continuous(breaks = seq(500, 2000, by = 250), limits = c(500,2000)) + 
  facet_wrap(~keyword, ncol = 6)
ggsave("export/tuinbouw_datering_probability_sum.png", dpi= 300, width=12, height = 12)


# veeteelt
date_keyword_veeteelt <- date_keyword %>% filter(thema == 'veeteelt')
ggplot(date_keyword_veeteelt, aes(y=keyword)) + 
  geom_point(aes(x=per_mid, size=waardering), alpha=0.25) + scale_size_continuous(range = c(0.1, 2)) +
  geom_dumbbell(aes(x=begin, xend=eind), alpha=0.1, size_x =0.1, size_xend=0.1) +
  scale_x_continuous(breaks = seq(500, 2000, by = 250), limits = c(500,2000)) + 
  xlab("Datering")
ggsave("export/date_keyword_veeteelt.png", dpi= 300, width=12, height = 8)

keyword_period_prob %>% filter(thema == "veeteelt") %>%
  ggplot(aes(x=year, y=probability)) + geom_path() + 
  scale_x_continuous(breaks = seq(500, 2000, by = 250), limits = c(500,2000)) + 
  facet_wrap(~keyword, ncol = 6)
ggsave("export/veeteelt_datering_probability.png", dpi= 300, width=12, height = 12)

keyword_period_prob_sum %>% filter(thema == "veeteelt") %>%
  ggplot(aes(x=year, y=totaal)) + geom_path() + 
  scale_x_continuous(breaks = seq(500, 2000, by = 250), limits = c(500,2000)) + 
  facet_wrap(~keyword, ncol = 6)
ggsave("export/veeteelt_datering_probability_sum.png", dpi= 300, width=12, height = 12)


# visserij
date_keyword_vis <- date_keyword %>% filter(thema == 'vis')
ggplot(date_keyword_vis, aes(y=keyword)) + 
  geom_point(aes(x=per_mid, size=waardering), alpha=0.25) + scale_size_continuous(range = c(0.1, 2)) +
  geom_dumbbell(aes(x=begin, xend=eind), alpha=0.1, size_x =0.1, size_xend=0.1) +
  scale_x_continuous(breaks = seq(500, 2000, by = 250), limits = c(500,2000)) + 
  xlab("Datering")
ggsave("export/date_keyword_vis.png", dpi= 300, width=12, height = 8)

keyword_period_prob %>% filter(thema == "vis") %>%
  ggplot(aes(x=year, y=probability)) + geom_path() + 
  scale_x_continuous(breaks = seq(500, 2000, by = 250), limits = c(500,2000)) + 
  facet_wrap(~keyword, ncol = 3)
ggsave("export/vis_datering_probability.png", dpi= 300, width=12, height = 12)

keyword_period_prob_sum %>% filter(thema == "vis") %>%
  ggplot(aes(x=year, y=totaal)) + geom_path() +
  scale_x_continuous(breaks = seq(500, 2000, by = 250), limits = c(500,2000)) + 
  facet_wrap(~keyword, ncol = 3)
ggsave("export/vis_datering_probability_sum.png", dpi= 300, width=12, height = 12)
