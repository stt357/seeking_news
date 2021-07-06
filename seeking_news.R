library(stringr)
library(readr)
library(dplyr)
library(tidytext)
library(tidyr)

setwd("~/projects/seeking_news/")

sentiment_data <- data.frame(date_str=character(),
                 mean_sent=as.numeric(), 
                 sum_sent=as.numeric(), 
                 stringsAsFactors=FALSE) 

file_list <- list.files("./data/", pattern = "*.txt")
for (file in file_list) {
  print(file)

  # 15 is example of bad data robot check
  news <- read_file(paste("./data/", file, sep = ""))
  news_lines <- strsplit(gsub("\n", "|", news), "|", fixed = T) # for some reason couldn't split on \n
  news_date <- news_lines[[1]][104] # hopefully a consistent date line to use
  #stringr str_detect to test for month name to ensure we have a date, str_split to get parts
  news_lines[[1]][1:100] = "" # getting rid of header text and link names
  news_lines[[1]][(length(news_lines[[1]]) - 40):length(news_lines[[1]])] = ""
  
  text_df <- tibble(text = news_lines[[1]])
  
  tokens <- text_df %>%
    unnest_tokens(word, text)
  
  sentiment <- get_sentiments("afinn")
  bing_sent <- get_sentiments("bing")
  
  word_sentiments <- tokens %>%
    filter(!word %in% c("cancer", "crude", "gross", "jobless", 
                        "break", "vice", "cloud", "junk")) %>%
    inner_join(bing_sent) %>%
    count(word, sentiment, sort = T) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment = positive - negative)
  print(news_date)
  sentiment_data[nrow(sentiment_data) + 1,] = c(news_date,mean(word_sentiments$sentiment), sum(word_sentiments$sentiment))
}

sent_data <- sentiment_data %>%
  group_by(date_str) %>%
  mutate(mean_sent = mean(as.numeric(mean_sent)), sum_sent = sum(as.numeric(sum_sent))) %>%
  distinct()

library("ggpubr")
sent_means <- c(0.7165, 0.8655, 0.835)
sp500 <- c(26.16, 19.57, −32.36)

sent_means <- c(0.7165, 0.8655, 0.835)
sent_change <- c(355, 309, 329)
sp500 <- c(3361.29,3380.86,3348.69)

cor(sent_means, sp500, method = "pearson")
cor.test(sent_change, sp500, method = "pearson")

sent_price <- data.frame(sentiment = sent_change, points_change = sp500)

library(corrgram)
corrgram(sent_price, order=TRUE)

ggscatter(sent_price, x = "sentiment", y = "points_change", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson")

