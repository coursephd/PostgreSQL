
https://github.com/trinker/sentimentr
https://epjdatascience.springeropen.com/articles/10.1140/epjds/s13688-017-0121-9 -- very good example


library(sentimentr)
library(magrittr)
library(dplyr)

sec014 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/sec014.rds")

anals <- sec014 %>%
  mutate(dialogue_split = get_sentences(sec014$sec014_var001_Complaint)) %$%
  sentiment_by(get_sentences(dialogue_split), by = list(mr_no, vis)) 

anals_notes <- sec014 %>%
  mutate(dialogue_split = get_sentences(sec014$`sec014_var002_Doctor's Notes`)) %$%
  sentiment_by(get_sentences(dialogue_split), by = list(mr_no, vis))
  

sec014 %>%
  mutate(dialogue_split = get_sentences(sec014$sec014_var001_Complaint)) %$%
  sentiment_by(get_sentences(dialogue_split), by = list(mr_no, vis)) %>%
  highlight("D:/Hospital_data/ProgresSQL/analysis/sec014_sentiment.html")
