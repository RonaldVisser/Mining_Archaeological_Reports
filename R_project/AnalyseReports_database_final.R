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
library(getPass)


# used sources for inspiration: 
# http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know 
#
# 1. Choose folder with document
# organisation of data: folder per city / pdf-filename= OMnr_City_Location.pdf
# select a single document in folder with all documents (cross platform)
folder_docs <- tk_choose.dir(default = "", caption = "Select directory with pdf reports")
setwd(folder_docs)
sub_dirs <- list.dirs()
# connect to database
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, user='postgres', password=getPass::getPass(), dbname='stadslandbouw')
#
dutch <- tesseract("nld")
# 2. Read documents
for(j in 1:length(sub_dirs)) {
  setwd(file.path(folder_docs, sub_dirs[j]))
  file_names <- list.files(pattern = "\\.pdf$", ignore.case=TRUE)
  # If there are no PDFs in folder, go to next folder
  if (length(file_names)==0) {next}
  wordcloud_names <- gsub('.pdf', '_wordcloud.jpg', stri_enc_toascii(file_names))
  for(i in 1:length(file_names)) {
    doc_fonts <- pdf_fonts(file_names[i])
    # check for fonts: if no fonts then OCR must be don 
    if (nrow(doc_fonts)==0) {
      txt_docs <- tesseract::ocr(file_names[i], engine = dutch)
    } else {
      txt_docs <- pdf_text(file_names[i])
    }
    doc_info <- pdf_info(file_names[i])
    doc_frame <- data.frame(filename=file_names[i], path=getwd(), pages=doc_info[2], zaak_id=as.numeric(substr(file_names[i], 1, 10)), city=substring(sub_dirs[j],3))
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
    d <- data.frame(word = names(v),freq=v,filename=rep(file_names[i],length(v)),row.names = NULL)
    set.seed(1234)
    if (length(v) > 0){
      jpeg(wordcloud_names[i])
      wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
      dev.off()
    }
    # write to database
    dbWriteTable(con, "tbl_term_docs", d, append=TRUE, row.names = FALSE)
  }
}
