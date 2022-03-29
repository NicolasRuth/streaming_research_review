library(tidyverse)

### Import data ###

articles_data <- readxl::read_excel("./data/articles_on_streaming.xlsx")

articles_data$year <- as.factor(articles_data$year)

# Articles per year

articles_per_year <- ggplot(articles_data, 
                            aes(as.factor(year),
                            fill = as.factor(year))
                            ) +
  geom_histogram(stat = "count", show.legend = F) +
  scale_fill_brewer(palette = "YlOrRd") +
  labs(x = "Jahr", y = "Anzahl der Artikel") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14)) 

articles_per_year

# Most frequent methods

methods <- rbind(articles_data$method, 
                 articles_data$method2, 
                 articles_data$method3)

rev(sort(table(methods)))

# Most frequent Journals

journals <- articles_data$journal
rev(sort(table(journals)))
length(unique(journals))

# Topic frequency
table(articles_data$Thema)
