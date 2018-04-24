library(tidytext)
library(tidyverse)

data <- read.csv("FAKE_DATA.csv", stringsAsFactors = FALSE)
data$Chief.Narrative <- gsub(' +',' ', data$Chief.Narrative)
data$Chief.Narrative <- gsub(' he ',' ', data$Chief.Narrative)
data$Chief.Narrative <- gsub(' her ',' ', data$Chief.Narrative)
data$Chief.Narrative <- gsub(' his ',' ', data$Chief.Narrative)
data$Chief.Narrative <- gsub(' hers ',' ', data$Chief.Narrative)
data$Chief.Narrative <- gsub(' yom ',' ', data$Chief.Narrative)
data$Chief.Narrative <- gsub(' yof ',' ', data$Chief.Narrative)
data$Chief.Narrative <- gsub(' female ',' ', data$Chief.Narrative)
data$Chief.Narrative <- gsub(' male ',' ', data$Chief.Narrative)
data$Chief.Narrative <- gsub(' yof ',' ', data$Chief.Narrative)
data$Chief.Narrative <- gsub(' female ',' ', data$Chief.Narrative)
data$Chief.Narrative <- gsub(' male ',' ', data$Chief.Narrative)
data$Chief.Narrative <- gsub(' mgh ',' ', data$Chief.Narrative)

od_words <- data %>%
  filter(man.od == 1) %>% 
  select(Chief.Narrative) %>%
  unnest_tokens(word, Chief.Narrative) 

od_words <- od_words$word

for (i in 1:9) {
  
  nums <- sample(1:1289, 184)
  temp <- od_words[nums]
  paragraph <- paste(temp, collapse = " ")
  data[i, 6] <- paragraph
  
}

non_od_words <- data %>%
  filter(man.od == 0) %>% 
  select(Chief.Narrative) %>%
  unnest_tokens(word, Chief.Narrative) 

non_od_words <- non_od_words$word

for (i in 10:19) {
  
  nums <- sample(1:1266, 184)
  temp <- od_words[nums]
  paragraph <- paste(temp, collapse = " ")
  data[i, 6] <- paragraph
  
}

data$Chief.Narrative <- gsub('NA','', data$Chief.Narrative)

write.csv(data, "FAKE_DATA_heim.csv")
