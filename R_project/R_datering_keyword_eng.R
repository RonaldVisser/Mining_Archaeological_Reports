library(readxl)
library(tidyverse)
library(ggalt)
library(ggpubr)
library(showtext)
library(svglite)

options(scipen = 999)

trace(grDevices::svg, exit = quote({showtext::showtext_begin()}), print = FALSE)
# see trace: https://stackoverflow.com/questions/34522732/changing-fonts-in-ggplot2
font_add("RijksoverheidSerif-Regular_2", "RijksoverheidSerif-Regular_2.otf")
showtext_auto()

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
ggplot(period_prob_sum,aes(x=year, y=totaal)) + geom_path() + xlim(500,2000) + 
  ylab("summarized probility") + theme_grey(base_family = "RijksoverheidSerif-Regular_2")
ggsave("export/English/date_density_sum.svg", dpi= 300, width=12, height = 8)

# summarized probability density of theme's
thema_period_prob_sum <- keyword_period_prob %>% group_by(thema,year) %>% summarise(totaal = sum(as.numeric(probability)))
# summarized probability density of keywords (by theme)
keyword_period_prob_sum <- keyword_period_prob %>% group_by(thema,keyword,year) %>% summarise(totaal = sum(as.numeric(probability)))

# example to explain method
selection_example <- keyword_period_prob[1:2150,] %>% filter (thema=="tuinbouw")
ggplot(selection_example, aes(x=year, y=probability, color = keyword)) + geom_path() + 
  ylim(0,0.006) + theme_grey(base_family = "RijksoverheidSerif-Regular_2")
ggsave("export/English/example_year_density.svg", dpi= 300, width=12, height = 8)
selection_example_sum <- keyword_period_prob[1:2150,] %>% filter (thema=="tuinbouw") %>% group_by(year) %>% summarise(probability = sum(as.numeric(probability)))
selection_example_sum <- cbind(rep("probability_sum",length(selection_example_sum)),rep("probability_sum",length(selection_example_sum)),selection_example_sum)
colnames(selection_example_sum) <- colnames(selection_example) <- c("keyword","thema", "year", "probability")
selection_example <- rbind(selection_example, selection_example_sum)
rm(selection_example_sum)
ggplot(selection_example, aes(x=year, y=probability, color = keyword)) + geom_path() + ylim(0,0.006) + 
  ylab("summarized probability density") + theme_grey(base_family = "RijksoverheidSerif-Regular_2")
ggsave("export/English/example_year_density_sum.svg", dpi= 300, width=12, height = 8)

# totalen (x-limit 500-2000 na Chr.)
ggplot(keyword_period_prob,aes(x=year, y=probability)) + geom_path() + 
  scale_x_continuous(breaks = seq(500, 2000, by = 250), limits = c(500,2000)) + 
  ylab("Probability density") + theme_grey(base_family = "RijksoverheidSerif-Regular_2") + 
  facet_wrap(~thema, ncol = 2, 
             labeller = labeller(thema = c("akkerbouw" = "arable farming",
                                           "algemeen" = "general keywords",
                                           "boomgaard" = "orchards",
                                           "tuinbouw" = "horticulture",
                                           "veeteelt" = "animal husbandry",
                                           "vis" = "fish")))
ggsave("export/English/thema_year_density.svg", dpi= 300, width=12, height = 8)

ggplot(thema_period_prob_sum,aes(x=year, y=totaal)) + geom_path() + 
  scale_x_continuous(breaks = seq(500, 2000, by = 250), limits = c(500,2000)) + 
  facet_wrap(~thema, ncol = 2, 
             labeller = labeller(thema = c("akkerbouw" = "arable farming",
                                           "algemeen" = "general keywords",
                                           "boomgaard" = "orchards",
                                           "tuinbouw" = "horticulture",
                                           "veeteelt" = "animal husbandry",
                                           "vis" = "fish"))) +
  ylab("summarized probability density") + theme_grey(base_family = "RijksoverheidSerif-Regular_2")
ggsave("export/English/thema_year_density_sum.svg", dpi= 300, width=12, height = 8)

for (th in c("akkerbouw", "algemeen","boomgaard","tuinbouw","veeteelt", "vis")){
  individual <- keyword_period_prob %>% filter(thema==th) %>%
    ggplot(aes(x=year, y=probability)) + geom_path() + ylab("Probability density") + 
    scale_x_continuous(breaks = seq(500, 2000, by = 250), limits = c(500,2000)) +
    theme_grey(base_family = "RijksoverheidSerif-Regular_2")
  summarized <- thema_period_prob_sum %>% filter(thema==th) %>%
    ggplot(aes(x=year, y=totaal)) + geom_path() + 
    scale_x_continuous(breaks = seq(500, 2000, by = 250), limits = c(500,2000)) + 
    ylab("summarized probability density") + theme_grey(base_family = "RijksoverheidSerif-Regular_2")
  ggarrange(individual,summarized, nrow=2)
  ggsave(paste0("export/English/Theme_", th, "_year_density.svg"), dpi= 300, width=12, height = 16)
  date_keyword %>% filter(thema == th) %>%
    ggplot(aes(y=keyword)) +
    geom_point(aes(x=per_mid, size=waardering), alpha=0.25) + scale_size_continuous(name = "Rating", range = c(0.1, 2)) +
    geom_dumbbell(aes(x=begin, xend=eind), alpha=0.1, size_x =0.1, size_xend=0.1) +
    scale_x_continuous(breaks = seq(500, 2000, by = 250), limits = c(500,2000)) + 
    xlab("Year") + theme_grey(base_family = "RijksoverheidSerif-Regular_2")
  ggsave(paste0("export/English/date_keyword_", th, ".svg"), dpi= 300, width=12, height = 8)
  keyword_period_prob %>% filter(thema == th) %>%
    ggplot(aes(x=year, y=probability)) + geom_path() +
    scale_x_continuous(breaks = seq(500, 2000, by = 250), limits = c(500,2000)) + 
    facet_wrap(~keyword, ncol = 6) + ylab("Probability density") + theme_grey(base_family = "RijksoverheidSerif-Regular_2")
  ggsave(paste0("export/English/", th, "_year_density.svg"), dpi= 300, width=12, height = 12)
  keyword_period_prob_sum %>% filter(thema == "akkerbouw") %>%
    ggplot(aes(x=year, y=totaal)) + geom_path() + 
    scale_x_continuous(breaks = seq(500, 2000, by = 250), limits = c(500,2000)) + 
    facet_wrap(~keyword, ncol = 6)+ ylab("summarized probability density") + 
    theme_grey(base_family = "RijksoverheidSerif-Regular_2")
  ggsave(paste0("export/English/", th, "_year_density_sum.svg"), dpi= 300, width=12, height = 12)
}
