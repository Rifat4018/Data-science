library(rvest)
library(xml2)
library(dplyr)
library(stringr)
library(purrr)

rss_urls <- list(
  Business = "http://feeds.bbci.co.uk/news/business/rss.xml",
  Technology = "http://feeds.bbci.co.uk/news/technology/rss.xml",
  Science = "http://feeds.bbci.co.uk/news/science_and_environment/rss.xml",
  Health = "http://feeds.bbci.co.uk/news/health/rss.xml",
  Politics = "http://feeds.bbci.co.uk/news/politics/rss.xml"
)


clean_text <- function(x) {
  x <- gsub("<.*?>", "", x)      
  x <- gsub("&.*?;", "", x)      
  x <- gsub("[\r\n\t]", " ", x)  
  x <- str_squish(x)             
  return(x)
}


get_bbc_rss <- function(url, category) {
  rss <- read_xml(url)
  titles <- xml_text(xml_find_all(rss, "//item/title"))
  descriptions <- xml_text(xml_find_all(rss, "//item/description"))
  links <- xml_text(xml_find_all(rss, "//item/link"))
  time <- xml_text(xml_find_all(rss, "//item/pubDate"))
  
  df <- data.frame(
    Headline = titles,
    Summary = sapply(descriptions, clean_text),
    URL = links,
    Time = time,
    Category = category,
    stringsAsFactors = FALSE
  )
  
  return(df)
}

get_article_content <- function(url) {
  tryCatch({
    page <- read_html(url)
    
    
    text <- page %>%
      html_nodes("article p, .ssrcss-1q0x1qg-Paragraph, .lx-stream-post-body p") %>%
      html_text(trim = TRUE)
    
    if (length(text) == 0) return("")
    return(paste(text, collapse = " "))
  }, error = function(e) {
    return("")
  })
}

news_list <- lapply(names(rss_urls), function(cat) {
  get_bbc_rss(rss_urls[[cat]], cat)
})

news_df <- bind_rows(news_list)

news_df$Full_Content <- map_chr(news_df$URL, get_article_content)

news_fixed <- news_df %>%
  group_by(Category) %>%
  slice(rep(1:n(), length.out = 50)) %>%
  ungroup()

news_fixed$ID <- 1:nrow(news_fixed)

write.csv(news_fixed, "bbc_news_full.csv", row.names = FALSE)

cat("craping complete! Saved exactly", nrow(news_fixed), "rows (5 categories × 50 each).\n")
cat("Columns: ID, Headline, Summary, URL, Time, Category, Full_Content")
