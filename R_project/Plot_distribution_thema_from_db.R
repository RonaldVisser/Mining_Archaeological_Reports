library(RPostgreSQL)
library(ggplot2)
library(tcltk)
library(getPass)
library(showtext)
library(svglite)

# non scientific numbers                                                      
options(scipen=999)

# directories for export
dir.create("export", showWarnings = FALSE)
dir.create("export/theme_distribution", showWarnings = FALSE)
dir.create("export/cat_distribution", showWarnings = FALSE)

font_add("RijksoverheidSerif-Regular_2", "RijksoverheidSerif-Regular_2.otf")
showtext_auto()
trace(grDevices::svg, exit = quote({showtext::showtext_begin()}), print = FALSE)
# see trace: https://stackoverflow.com/questions/34522732/changing-fonts-in-ggplot2

# connect to database
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, user='postgres', password=getPass::getPass(), dbname='stadslandbouw')
# get all thema from database
thema_list = dbGetQuery(con, "select distinct thema from thema_sums order by thema")
for (thema in thema_list$thema) {
  SQL = paste0("select zaak_id, filename, sum_index from thema_sums where thema ='", thema,"';", collapse = "")
  thema_data = dbGetQuery(con, SQL)
  # nr bins histogram
  nr_bins <- round(sqrt(nrow(thema_data)),0)
  ggplot(thema_data, aes(x = sum_index)) + geom_histogram(bins = nr_bins) + scale_x_log10() + 
    xlab("Score") + ylab("Count") + theme_grey(base_family = "RijksoverheidSerif-Regular_2")
  ggsave(paste0("export/theme_distribution/", thema,"_distribution_x_log10.svg", collapse=""))
  ggplot(thema_data, aes(x = sum_index)) + geom_histogram(bins = nr_bins) + scale_x_sqrt() + 
    xlab("Score") + ylab("Count") + theme_grey(base_family = "RijksoverheidSerif-Regular_2")
  ggsave(paste0("export/theme_distribution/", thema,"_distribution_x_sqrt.svg", collapse = ""))
  ggplot(thema_data, aes(x = sum_index)) + geom_histogram(bins = nr_bins) + xlab("Score") + 
    ylab("Count") + theme_grey(base_family = "RijksoverheidSerif-Regular_2")
  ggsave(paste0("export/theme_distribution/", thema,"_distribution.svg", collapse = ""))
}

# get all categories from database
cat_list = dbGetQuery(con, "select distinct category from tbl_keywords order by category")
for (category in cat_list$category) {
  SQL = paste0("select zaak_id, filename, indexvalue from rapport_totaalscore_keywords_category where category ='", category,"';", collapse = "")
  cat_data = dbGetQuery(con, SQL)
  cat_filename <- gsub("/", "_", category)
  # nr bins histogram
  nr_bins <- round(sqrt(nrow(cat_data)),0)
  ggplot(cat_data, aes(x = indexvalue)) + geom_histogram(bins = nr_bins) + 
    scale_x_log10() + xlab("Score") + ylab("Count") + theme_grey(base_family = "RijksoverheidSerif-Regular_2")
  ggsave(paste0("export/cat_distribution/", cat_filename,"_distribution_x_log10.svg", collapse=""))
  ggplot(cat_data, aes(x = indexvalue)) + geom_histogram(bins = nr_bins) + 
    scale_x_sqrt() + xlab("Score") + ylab("Count") + theme_grey(base_family = "RijksoverheidSerif-Regular_2")
  ggsave(paste0("export/cat_distribution/", cat_filename,"_distribution_x_sqrt.svg", collapse = ""))
  ggplot(cat_data, aes(x = indexvalue)) + geom_histogram(bins = nr_bins) + 
    xlab("Score") + ylab("Count") + theme_grey(base_family = "RijksoverheidSerif-Regular_2")
  ggsave(paste0("export/cat_distribution/", cat_filename,"_distribution.svg", collapse = ""))
}

