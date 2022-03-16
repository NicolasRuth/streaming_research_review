library(tm)
library(wordcloud)

### Word Cloud ###

text <- paste(articles_data$key1,
              articles_data$key2,
              articles_data$key3,
              articles_data$key4,
              articles_data$key5,
              articles_data$key6,
              articles_data$key7,
              articles_data$key8,
              articles_data$key9,
              articles_data$key10,
              sep = " ")

text <- gsub("NA ", "", text)
text <- gsub("NA", "", text)
text <- gsub("music", "", text)
text <- gsub("streaming", "", text)
text <- gsub("Music", "", text)
text <- gsub("Streaming", "", text)


docs <- Corpus(VectorSource(text))

docs <- docs %>% 
  tm_map(removeNumbers) %>% 
  tm_map(removePunctuation) %>% 
  tm_map(stripWhitespace)

docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))  

dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix), 
              decreasing=TRUE) 
df <- data.frame(word = names(words),
                 freq = words)

set.seed(230721)

wordcloud(words = df$word, freq = df$freq, min.freq = 3,
          max.words = 100, random.order = FALSE, rot.per = 0.35,            
          colors = brewer.pal(8, "Dark2"),
          scale = c(6, 0.4))
