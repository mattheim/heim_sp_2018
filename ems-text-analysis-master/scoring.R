#### Read in data
library(tidyverse)
library(tidytext)
modeling <- read.csv("modeling.csv", stringsAsFactors = FALSE)
#####

##### PI Scoring
#od_pi_terms <- read.csv("od_pi_terms.csv", stringsAsFactors = FALSE)
modeling["pi_score"] <- sapply(modeling$Primary.Impression, function(x) as.integer(x %in% od_pi_terms$Primary.Impression))
#####

#### CC Scoring
modeling <- mutate(modeling, cc_w_score = 0) # initialze column
drug_terms <- c("heroin", "fentanyl", "methadone", "narcan", "overdose", "heroine", "od")
cc_w_colnum = which(colnames(modeling) == "cc_w_score")

for (i in 1:nrow(modeling)) {
  
  temp <- data_frame(text = modeling[i, 19]) # make each CC a df
  temp_words <- unnest_tokens(temp, word, text) # tokenize each df
  temp_words <- tolower(temp_words$word) # df to vector
  
  x <- base::intersect(temp_words, drug_terms)  
  
  x <- length(x)
  
  modeling[i, cc_w_colnum] <- x
  print(i)
}
####

#### CN Word Scoring
#od_cn_words <- read.csv("od_cn_words.csv", stringsAsFactors = FALSE)
modeling <- mutate(modeling, cn_w_score = 0) # initialze column
cn_w_colnum = which(colnames(modeling) == "cn_w_score")

for (i in 1:nrow(modeling)) {
  
  temp <- data_frame(text = modeling[i, 20]) # make each CN a df
  temp_words <- unnest_tokens(temp, word, text) # tokenize each df
  temp_words <- tolower(temp_words$word) # df to vector
  
  x <- base::intersect(temp_words, od_cn_words$word)  
  
  x <- od_cn_words %>%
    filter(word %in% x) 
  
  x <- sum(x$score)
  
  modeling[i, cn_w_colnum] <- x
  print(i)
}

####

#### CN Bigram Scoring

#od_cn_bigrams <- read.csv("od_cn_bigrams.csv", stringsAsFactors = FALSE)
modeling <- mutate(modeling, cn_bg_score = 0) # initialze column
cn_bg_colnum = which(colnames(modeling) == "cn_bg_score")

for (i in 1:nrow(modeling)) {
  
  temp <- data_frame(text = modeling[i, 20]) # make each CN a df
  temp_words <- unnest_tokens(temp, word, text, token = "ngrams", n = 2) # tokenize each df
  temp_words <- tolower(temp_words$word) # df to vector
  
  x <- base::intersect(temp_words, od_cn_bigrams$word) # words in CN that are high value  
  
  x <- od_cn_bigrams %>% # filter for scores
    filter(word %in% x) 
  
  x <- sum(x$score) # sum of scores
  
  modeling[i, cn_bg_colnum] <- x # assign score
  print(i)
} 

####

#### CN trigram Scoring

modeling <- data

od_cn_trigrams <- read.csv("od_cn_trigrams.csv", stringsAsFactors = FALSE)
modeling <- mutate(modeling, cn_tg_score = 0) # initialze column
cn_tg_colnum = which(colnames(modeling) == "cn_tg_score")

for (i in 1:nrow(modeling)) {
  
  temp <- data_frame(text = modeling[i, 20]) # make each CN a df
  temp_words <- unnest_tokens(temp, word, text, token = "ngrams", n = 3) # tokenize each df
  temp_words <- tolower(temp_words$word) # df to vector
  
  x <- base::intersect(temp_words, od_cn_trigrams$word) # words in CN that are high value  
  
  x <- od_cn_trigrams %>% # filter for scores
    filter(word %in% x) 
  
  x <- sum(x$score) # sum of scores
  
  modeling[i, cn_tg_colnum] <- x # assign score
  print(i)
  
} 

#####

####

modeling <- mutate(modeling, cn_d_score = 0) # initialze column
drug_terms <- c("heroin", "fentanyl", "methadone", "narcan", "overdose", "heroine", "od")
cn_d_colnum = which(colnames(modeling) == "cn_d_score")

for (i in 1:nrow(modeling)) {
  
  temp <- data_frame(text = modeling[i, 20]) # make each CC a df
  temp_words <- unnest_tokens(temp, word, text) # tokenize each df
  temp_words <- tolower(temp_words$word) # df to vector
  
  x <- base::intersect(temp_words, drug_terms)  
  
  x <- length(x)
  
  modeling[i, cn_d_colnum] <- x
  print(i)
}


####


write.csv(modeling, "C:/Users/anpat/modeling_scored.csv")

#####

#modeling <- read.csv("modeling_scored.csv", stringsAsFactors = FALSE)
write.csv(modeling, "modeling_scored.csv")


