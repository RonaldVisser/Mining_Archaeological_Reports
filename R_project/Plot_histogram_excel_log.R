library(readxl)
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(svglite)
library(showtext)

font_add("RijksoverheidSerif-Regular_2", "RijksoverheidSerif-Regular_2.otf")
showtext_auto()
trace(grDevices::svg, exit = quote({showtext::showtext_begin()}), print = FALSE)


Rapport_totaalscore_keywords <- read_excel("data/Rapport_totaalscore_keywords_20180717.xlsx")
#Rapport_totaalscore_keywords <- Rapport_totaalscore_keywords_20180821

Rapport_totaalscore_keywords <- Rapport_totaalscore_keywords %>% 
  mutate(log10index = log10(indexvalue), lnindex = log(indexvalue))

# non scientific numbers                                                      
options(scipen=999)
totaal_index <- ggplot(Rapport_totaalscore_keywords, aes(x = indexvalue)) + geom_histogram() + theme_grey(base_family = "RijksoverheidSerif-Regular_2")
log_totaal_index <- ggplot(Rapport_totaalscore_keywords, aes(x = log10index)) + geom_histogram() + xlab("log10(indexvalue)") + theme_grey(base_family = "RijksoverheidSerif-Regular_2")
ln_totaal_index <- ggplot(Rapport_totaalscore_keywords, aes(x = lnindex)) + geom_histogram() + xlab("ln(indexvalue)") + theme_grey(base_family = "RijksoverheidSerif-Regular_2")

ggarrange(totaal_index,log_totaal_index)
ggsave("export/histogram_report_totalscores.svg", width = 12, height = 6 )

# Kernel Density Estimation
#plot(density(log(Rapport_totaalscore_keywords$indexvalue))) # default
ggplot(Rapport_totaalscore_keywords, aes(x = indexvalue)) + geom_density() + scale_x_log10()
ggplot(Rapport_totaalscore_keywords, aes(x = log10(indexvalue))) + geom_density()
# combine histogram and KDE
ggplot(Rapport_totaalscore_keywords, aes(x = indexvalue)) + geom_histogram() + geom_density(aes(y= 0.2 * ..count..), color = "red")
ggplot(Rapport_totaalscore_keywords, aes(x = log10index)) + geom_histogram() + geom_density(aes(y= 0.2 * ..count..), color = "red")


