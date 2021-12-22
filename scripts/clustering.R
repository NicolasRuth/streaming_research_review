library(tidyverse)
library(tidytext)
library(widyr)
library(tm)
library(topicmodels)

### Text clustering

stream_title <- tibble(id = articles_data$ID, 
                       title = articles_data$title) %>% unnest(title)

stream_key <- tibble(id = articles_data$ID, 
                     key = articles_data$key1) %>% unnest(key)

stream_abstract <- tibble(id = articles_data$ID, 
                          abstract = articles_data$abstract) %>% unnest(abstract)

## Set stopwords

stream_title <- stream_title %>% 
  unnest_tokens(word, title) 

stream_key <- stream_key %>% 
  unnest_tokens(word, key) 

stream_abstract <- stream_abstract %>% 
  unnest_tokens(word, abstract)

# Check for most frequent words
stream_abstract %>% count(word, sort = TRUE)

my_stopwords <- tibble(word = c("music", "streaming", "services", 
                                "article","spotify", "based", "study", 
                                "results", "research", "paper", 
                                "findings", "digital"))

stream_abstract <- stream_abstract %>% 
  anti_join(my_stopwords)

stream_title <- stream_title %>% 
  anti_join(my_stopwords)

## Title word correlations

title_word_pairs <- stream_title %>% 
  pairwise_count(word, id, sort = TRUE, upper = FALSE)

abstract_word_pairs <- stream_abstract %>% 
  pairwise_count(word, id, sort = TRUE, upper = FALSE)

title_tf_idf <- stream_title %>% 
  count(id, word, sort = TRUE) %>%
  ungroup() %>%
  bind_tf_idf(word, id, n)

title_tf_idf <- full_join(title_tf_idf, stream_key, by = "id")

abstract_tf_idf <- stream_abstract %>% 
  count(id, word, sort = TRUE) %>%
  ungroup() %>%
  bind_tf_idf(word, id, n)

abstract_tf_idf <- full_join(abstract_tf_idf, stream_abstract, by = "id")

## Topic modelling

word_counts <- stream_abstract %>%
  anti_join(my_stopwords) %>%
  count(id, word, sort = TRUE) %>%
  ungroup()

abstract_dtm <- word_counts %>%
  cast_dtm(id, word, n)

abstract_lda <- LDA(abstract_dtm, k = 5, control = list(seed = 1234))

tidy_lda <- tidy(abstract_lda)

top_terms <- tidy_lda %>%
  group_by(topic) %>%
  slice_max(beta, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  group_by(topic, term) %>%    
  arrange(desc(beta)) %>%  
  ungroup() %>%
  ggplot(aes(beta, term, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  scale_y_reordered() +
  labs(title = "Top 10 terms in each streaming topic",
       x = expression(beta), y = NULL) +
  facet_wrap(~ topic, ncol = , scales = "free")
