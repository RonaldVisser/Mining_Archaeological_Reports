library(wordcloud)
library(RColorBrewer)
library(svglite)


keywords <- read.csv("data/Termen_voorkomen_totaal_20180827.csv")
terms <- read.csv("data/Word_count_totaal_20180827.csv")

svglite("export/wordcloud_keywords.svg")
wordcloud(words = keywords$word, freq = keywords$totaal_aantal, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
dev.off()

svglite("export/wordcloud_terms.svg")
wordcloud(words = terms$word, freq = terms$aantal, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
dev.off()
