
# https://github.com/trinker/sentimentr
# https://epjdatascience.springeropen.com/articles/10.1140/epjds/s13688-017-0121-9 -- very good example

library(sentimentr)
library(magrittr)
library(dplyr)
library(ggplot2)
library(data.table)

all_met_rmsd <- readRDS("D:/Hospital_data/ProgresSQL/analysis/01adsl_met_rmsd.rds")
all_met_rmsd <- unique(all_met_rmsd[, c("mr_no", "RMSD", "Metabolic", "combine", 
                                        "patient_gender", "all_vis", "cdur")])

sec014 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/sec014.rds")

anals <- sec014 %>%
  mutate(dialogue_split = get_sentences(sec014$sec014_var001_Complaint)) %$%
  sentiment_by(get_sentences(dialogue_split), by = list(mr_no, vis)) 

anals <- merge(x = anals,
               y = all_met_rmsd,
               all.x = TRUE,
               by = c("mr_no")  )


fwrite(anals, 
       "D:/Hospital_data/ProgresSQL/analysis/01_sentiment.csv")


anals_nrc <- sec014 %>%
  mutate(dialogue_split = get_sentences(sec014$sec014_var001_Complaint)) %$%
  sentiment_by(get_sentences(dialogue_split),
               polarity_dt = lexicon::hash_sentiment_nrc,
               by = list(mr_no, vis)) 



ggplot(data = anals, aes(x = vis, y = mr_no)) +
  geom_tile(aes(fill = ave_sentiment)) +
  scale_fill_gradient2()




sec004_1 <- merge(x = sec014,
                  y = anals,
                  all.x = TRUE,
                  by.x = c("mr_no", "vis"),
                  by.y = c("mr_no", "vis")  )





anals_notes <- sec014 %>%
  mutate(dialogue_split = get_sentences(sec014$`sec014_var002_Doctor's Notes`)) %$%
  sentiment_by(get_sentences(dialogue_split), by = list(mr_no, vis))


sec014 %>%
  mutate(dialogue_split = get_sentences(sec014$sec014_var001_Complaint)) %$%
  sentiment_by(get_sentences(dialogue_split), by = list(mr_no, vis)) %>%
  highlight("D:/Hospital_data/ProgresSQL/analysis/sec014_sentiment.html")
