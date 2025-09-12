required_packages <- c("tidyverse", "tm", "tidytext", "wordcloud", "SnowballC", 
                       "textclean", "syuzhet", "topicmodels", "ggthemes", "stringr")
packages_to_install <- required_packages[!required_packages %in% installed.packages()[, "Package"]]
if (length(packages_to_install) > 0) {
  install.packages(packages_to_install)
}
library(tidyverse)
library(tm)
library(tidytext)
library(wordcloud)
library(SnowballC)
library(textclean)
library(syuzhet)
library(topicmodels)
library(ggthemes)
library(stringr)

cat("All required packages have been loaded successfully!\n")

github_url <- "https://raw.githubusercontent.com/Rifat4018/Data-science/main/Final/bbc_news_final.csv"

raw_data <- read.csv(github_url, stringsAsFactors = FALSE)

cat("Number of rows in raw_data:", nrow(raw_data), "\n")
cat("Columns:", paste(colnames(raw_data), collapse = ", "), "\n")
table(raw_data$Category)  # Category distribution

raw_word_freq <- raw_data %>%
  unnest_tokens(word, Full_Content) %>%
  count(word, sort = TRUE)

ggplot(raw_word_freq[1:20, ], aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "darkred") +
  coord_flip() +
  labs(title = "Top 20 Words in Raw Data", x = "Word", y = "Count") +
  theme_minimal()

news_data <- raw_data

original_rows <- nrow(news_data)

str(news_data)
head(news_data)

sum(is.na(news_data))

clean_data <- function(df) {
  df <- df %>%
    filter(!is.na(Full_Content) & 
             Full_Content != "" & 
             nchar(Full_Content) > 50)
  
  df$Full_Content <- sapply(df$Full_Content, function(text) {
    text <- gsub("<.*?>", "", text)
    text <- gsub("[^[:alnum:][:space:],.!?;:]", "", text)
    text <- str_squish(text)
    return(text)
  })
  
  return(df)
}

news_data <- clean_data(news_data)

preprocess_text <- function(text) {
  text <- tolower(text)
  text <- removeNumbers(text)
  text <- removePunctuation(text)
  text <- stripWhitespace(text)
  text <- removeWords(text, stopwords("en"))
  text <- stemDocument(text)
  return(text)
}

news_data$Cleaned_Content <- sapply(news_data$Full_Content, preprocess_text)

news_data <- news_data %>%
  filter(!is.na(Cleaned_Content) & 
           Cleaned_Content != "" & 
           nchar(Cleaned_Content) > 10)

word_freq <- news_data %>%
  unnest_tokens(word, Cleaned_Content) %>%
  count(word, sort = TRUE) %>%
  filter(n > 50)  

ggplot(word_freq[1:20, ], aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 20 Most Frequent Words in BBC News", 
       x = "Word", y = "Frequency") +
  theme_minimal()

total_words_per_category <- news_data %>%
  unnest_tokens(word, Cleaned_Content) %>%
  group_by(Category) %>%
  summarise(total_words = n())

print(total_words_per_category)

ggplot(total_words_per_category, aes(x = reorder(Category, -total_words), 
                                     y = total_words, fill = Category)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Total Word Count per Category",
       x = "Category", y = "Total Words") +
  theme_minimal() +
  geom_text(aes(label = total_words), vjust = -0.5)

unigrams_cat <- news_data %>%
  unnest_tokens(unigram, Cleaned_Content, token = "words") %>%
  group_by(Category, unigram) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(Category, desc(n))

top_unigrams_cat <- unigrams_cat %>%
  group_by(Category) %>%
  slice_max(n, n = 10) %>%
  ungroup()

ggplot(top_unigrams_cat, aes(x = reorder(unigram, n), y = n, fill = Category)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = n), hjust = -0.1, size = 3) +
  coord_flip() +
  facet_wrap(~Category, scales = "free_y") +
  labs(title = "Top 10 Unigrams per Category", x = "Unigram", y = "Frequency") +
  theme_minimal()

bigrams_by_category <- news_data %>%
  unnest_tokens(bigram, Cleaned_Content, token = "ngrams", n = 2) %>%
  count(Category, bigram, sort = TRUE)

bigrams_by_category %>%
  group_by(Category) %>%
  slice_max(n, n = 10) %>%
  ungroup() %>%
  ggplot(aes(x = reorder_within(bigram, n, Category), y = n, fill = Category)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = n), hjust = -0.1, size = 3) +
  facet_wrap(~Category, scales = "free") +
  scale_x_reordered() +
  coord_flip() +
  labs(title = "Top 10 Bigrams per Category", 
       x = "Bigram", y = "Frequency") +
  theme_minimal()

categories <- unique(news_data$Category)

for (category in categories) {
  category_text <- news_data %>%
    filter(Category == category) %>%
    pull(Cleaned_Content) %>%
    paste(collapse = " ")
  
  wordcloud(category_text, max.words = 50, random.order = FALSE, 
            colors = brewer.pal(8, "Dark2"), main = paste("Word Cloud for", category))
}

category_words <- news_data %>%
  unnest_tokens(word, Cleaned_Content) %>%
  count(Category, word, sort = TRUE)

total_words <- category_words %>% 
  group_by(Category) %>% 
  summarize(total = sum(n))

category_words <- left_join(category_words, total_words)

category_tf_idf <- category_words %>%
  bind_tf_idf(word, Category, n) %>%
  arrange(desc(tf_idf))

category_tf_idf %>%
  group_by(Category) %>%
  slice_max(tf_idf, n = 10) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, reorder(word, tf_idf), fill = Category)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Category, scales = "free") +
  labs(x = "TF-IDF", y = NULL) +
  theme_minimal()

sentiment_scores <- get_nrc_sentiment(news_data$Full_Content)

news_data <- cbind(news_data, sentiment_scores)

sentiment_by_category <- news_data %>%
  group_by(Category) %>%
  summarise(across(anger:positive, mean)) %>%
  pivot_longer(cols = anger:positive, names_to = "sentiment", values_to = "value")

ggplot(sentiment_by_category, aes(x = sentiment, y = value, fill = Category)) +
  geom_col(position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Sentiment Analysis by Category", 
       x = "Sentiment", y = "Intensity") +
  theme_minimal()

dtm <- news_data %>%
  unnest_tokens(word, Cleaned_Content) %>%
  count(ID, word, sort = TRUE) %>%
  cast_dtm(ID, word, n)

row_total <- apply(dtm, 1, sum)
dtm <- dtm[row_total > 0, ]

lda_model <- LDA(dtm, k = 5, control = list(seed = 1234))

topics <- tidy(lda_model, matrix = "beta")

top_terms <- topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

cat("\n=== TOPIC MODELING INTERPRETATION ===\n")

top_terms_enhanced <- topics %>%
  group_by(topic) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

topic_labels <- c()
for (i in 1:5) {
  topic_terms <- top_terms_enhanced %>% 
    filter(topic == i) %>% 
    pull(term) %>% 
    head(8)
  
  cat("\nTopic", i, "Top Terms:", paste(topic_terms, collapse = ", "), "\n")
  
  if (any(c("game", "play", "seri", "gaming") %in% topic_terms)) {
    topic_label <- "Gaming & Entertainment"
  } else if (any(c("bank", "tax", "profit", "economi") %in% topic_terms)) {
    topic_label <- "Finance & Economy"
  } else if (any(c("health", "care", "patient", "medic") %in% topic_terms)) {
    topic_label <- "Healthcare"
  } else if (any(c("govern", "polit", "law", "state") %in% topic_terms)) {
    topic_label <- "Government & Politics"
  } else if (any(c("tech", "data", "secur", "encrypt") %in% topic_terms)) {
    topic_label <- "Technology & Security"
  } else {
    topic_label <- "General News"
  }
  
  topic_labels <- c(topic_labels, topic_label)
  cat("Interpretation:", topic_label, "\n")
}

top_terms_enhanced <- top_terms_enhanced %>%
  mutate(topic_label = factor(topic_labels[topic]))

top_terms_enhanced %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(term = reorder_within(term, beta, topic_label)) %>%
  ggplot(aes(beta, term, fill = topic_label)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic_label, scales = "free") +
  scale_y_reordered() +
  labs(title = "Top Terms in Each Topic with Interpretation", 
       x = "Beta", y = "Term") +
  theme_minimal()

enhanced_ner <- function(text) {
  persons <- str_extract_all(text, "\\b([A-Z][a-z]+\\s+[A-Z][a-z]+(?:\\s+[A-Z][a-z]+)*)\\b")[[1]]
  
  locations <- str_extract_all(text, "\\b([A-Z][a-z]+\\s*(?:City|Town|Village|Country|State|Province|County|Street|Road|Avenue|Boulevard|Park|Square|London|UK|US|USA|United States|United Kingdom|Europe|Asia|Africa)\\b)")[[1]]
  
  organizations <- str_extract_all(text, "\\b([A-Z][a-z]+\\s+(?:Inc|Corp|Ltd|Co|Company|Group|Holdings|Bank|University|College|Institute|Foundation)\\.?\\b)")[[1]]
  
  dates <- str_extract_all(text, "\\b(\\d{1,2}(?:st|nd|rd|th)?\\s+(?:January|February|March|April|May|June|July|August|September|October|November|December)\\s+\\d{4}|\\d{4}-\\d{2}-\\d{2}|\\d{1,2}/\\d{1,2}/\\d{4})\\b")[[1]]
  
  money <- str_extract_all(text, "\\b(?:£|\\$|€)\\s?\\d+(?:\\.\\d+)?\\s?(?:million|billion|thousand)?\\b|\\b\\d+(?:\\.\\d+)?\\s?(?:million|billion|thousand)?\\s?(?:pounds|dollars|euros)\\b")[[1]]
  
  return(list(
    persons = unique(persons), 
    locations = unique(locations), 
    organizations = unique(organizations),
    dates = unique(dates),
    money = unique(money)
  ))
}

sample_articles <- raw_data$Full_Content[1:10]

cat("\n=== NER COMPARISON: ENHANCED REGEX ===\n")
for (i in 1:length(sample_articles)) {
  enhanced_entities <- enhanced_ner(sample_articles[i])
  
  cat("\nENHANCED REGEX METHOD:\n")
  cat("Persons:", paste(enhanced_entities$persons, collapse = ", "), "\n")
  cat("Locations:", paste(enhanced_entities$locations, collapse = ", "), "\n")
  cat("Organizations:", paste(enhanced_entities$organizations, collapse = ", "), "\n")
  cat("Dates:", paste(enhanced_entities$dates, collapse = ", "), "\n")
  cat("Money:", paste(enhanced_entities$money, collapse = ", "), "\n")
  cat("========================================\n")
}

news_data$Full_Content <- sapply(news_data$Full_Content, function(text) {
  text <- gsub("[^[:alnum:][:space:],.!?;:]", " ", text)
  text <- str_squish(text)
  return(text)
})

news_data[] <- lapply(news_data, function(x) {
  if(is.character(x)) {
    x <- gsub("[^[:alnum:][:space:],.!?;:-]", " ", x)
    x <- str_squish(x)
  }
  return(x)
})

write.csv(news_data, "bbc_news_enriched_cleaned.csv", row.names = FALSE, fileEncoding = "UTF-8")

cat("NLP analysis complete! The cleaned and enriched dataset has been saved.\n")
cat("Original rows:", original_rows, "\n")
cat("Cleaned rows:", nrow(news_data), "\n")
cat("Removed rows:", original_rows - nrow(news_data), "\n")

cat("\n=== COMPREHENSIVE PROJECT SUMMARY ===\n")
cat("Categories analyzed:", paste(unique(news_data$Category), collapse = ", "), "\n")
cat("Total words processed:", sum(category_words$n), "\n")
cat("Unique words:", nrow(word_freq), "\n")
cat("Average sentiment by category:\n")
print(sentiment_by_category %>% group_by(Category) %>% summarise(avg_sentiment = mean(value[!sentiment %in% c("negative", "positive")])))