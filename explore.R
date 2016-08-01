library(tm); library(SnowballC); library(wordcloud)

ef <- emails_full

ef$body <- strsplit(ef$body, " ")
View(ef)
length(unique(ef$body))

efc <- Corpus(VectorSource(ef$body[1:300]))
efc <- tm_map(efc, PlainTextDocument)
efc <- tm_map(efc, removePunctuation)
efc <- tm_map(efc, removeWords, stopwords('english'))
efc <- tm_map(efc, stemDocument)
wordcloud(efc, 
          max.words = 100, 
          random.order = FALSE, 
          colors = brewer.pal(8, "Dark2"))
