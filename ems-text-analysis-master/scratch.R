#### CN word scores
training <- mutate(training, cn_w_score = 0) # initialze column
cn_w_colnum = which(colnames(training) == "cn_w_score")

for (i in 1:nrow(training)) {
  
  temp <- data_frame(text = training[i, 20]) # make each CN a df
  temp_words <- unnest_tokens(temp, word, text) # tokenize each df
  temp_words <- tolower(temp_words$word) # df to vector
  
  x <- base::intersect(temp_words, overdose_poswords$word)  
  
  x <- overdose_poswords %>%
    filter(word %in% x) 
  
  x <- sum(x$wscore)
  
  training[i, cn_w_colnum] <- x
  print(i)
}
####

