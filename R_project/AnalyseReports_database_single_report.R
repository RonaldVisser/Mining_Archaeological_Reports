# load libraries
library(tm)
library(pdftools)
library(wordcloud)
library(RColorBrewer)
library(dplyr)
library(RPostgreSQL)
library(tcltk)
library(tesseract)
library(stringi)

# used sources for inspiration: 
# https://stackoverflow.com/questions/8091303/simultaneously-merge-multiple-data-frames-in-a-list 
# http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know 
#
# 1. Choose folder with document
# organisation of data: folder per city / pdf-filename= OMnr_City_Location.pdf
# select a single document in folder with all documents (cross platform)
file_selected <- file.choose()
folder_docs <- dirname(file_selected)
setwd(folder_docs)
basename(folder_docs) # to get name of city
#file_names <- list.files(pattern = "\\.doc$|\\.docx$", ignore.case=TRUE)
# connect to database
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, user='postgres', password='rufio', dbname='stadslandbouw')
#
dutch <- tesseract("nld")
# 2. Read documents
doc_fonts <- pdf_fonts(file_selected)
# check for fonts: if no fonts then OCR must be don 
if (nrow(doc_fonts)==0) {
  txt_docs <- tesseract::ocr(file_selected, engine = dutch)
} else {
  txt_docs <- pdf_text(file_selected)
}
doc_info <- pdf_info(file_selected)
doc_frame <- data.frame(filename=file_selected, path=getwd(), pages=doc_info[2], om_nr=as.numeric(substr(file_selected, 1, 10)), city=substring(sub_dirs[j],3))
dbWriteTable(con, "tbl_docs", doc_frame, append=TRUE, row.names = FALSE)
docs <- Corpus(VectorSource(txt_docs))
# convert certain characters to space
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove dutch common stopwords
docs <- tm_map(docs, removeWords, stopwords("dutch"))
# Remove your own stop word
# Specify your stopwords as a character vector
#sel_stopwords <- c("aangetroffen", "onderzoek","mogelijk","gevonden","gevonden","archeologisch","archeologie")
#docs <- tm_map(docs, removeWords, sel_stopwords) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Build a term-document matrix
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v,filename=rep(file_selected,length(v)),row.names = NULL)
set.seed(1234)
if (length(v) > 0){
  jpeg(wordcloud_names[i])
  wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
  dev.off()
}
# write to database
dbWriteTable(con, "tbl_term_docs", d, append=TRUE, row.names = FALSE)

