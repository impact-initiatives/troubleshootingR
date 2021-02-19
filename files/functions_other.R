
# can be used as basis for translations, e.g. with gl_translate()
# requires vector with names of "other" text variables

pivot_longer_other <- function(df, vars_other) {
  
  require(tidyverse)
  
  list <- list()
  
  uuid <- "uuid"
  
  vars_other <- append(vars_other, uuid)
  
  other_data <- df[,vars_other]
  
  for (i in 1:nrow(other_data)) { 
    
    if(rowSums(is.na(other_data[i,])) < (ncol(other_data)-1)) {
      rdata <- other_data[i,] %>% pivot_longer(!uuid, 
                                               names_to = "variable",
                                               values_to = "text", 
                                               values_drop_na = T)
      
      list[[i]] <- rdata
      
    }
  } 
  
  other_data_long <- do.call(rbind, list)
  
  dup <- other_data_long[,c("uuid","variable")] 
  
  other_data_long <- other_data_long[!duplicated(dup) & 
                                       !duplicated(dup, fromLast=TRUE),]
  return(other_data_long)
  
} 

# identifies common word stems in "other" text responses 
# lowfreq_rel sets the minimum relative frequency per variable/question (default = 0.05), and 
# lowfreq_abs sets the minumum absolute frequency a word should have (default = 20)

identify_common_words <- function(df, vars_other, lowfreq_rel = 0.05, lowfreq_abs = 20) {
  
  require(tidyverse)
  require(tm)
  
  list = list()
  
  uuid <- "uuid"
  
  vars_other <- append(vars_other, uuid)
  
  other_data <- df[,vars_other]
  
  for (c in 1:ncol(other_data)) { 
    
    n <- sum(!is.na(other_data[c]))
    
    text <- Corpus(VectorSource(na.omit(other_data[c])))
    
    toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
    
    text <- tm_map(text, toSpace, "/")
    
    text <- tm_map(text, toSpace, "@")
    
    text <- tm_map(text, toSpace, "\\|")
    
    text <- tm_map(text, content_transformer(tolower))
    
    # Remove numbers
    
    text <- tm_map(text, removeNumbers)
    
    # Remove common English stopwords
    
    text <- tm_map(text, removeWords, stopwords("english"))
    text <- tm_map(text, removeWords, "the")
    
    # Remove punctuations
    
    text <- tm_map(text, removePunctuation)
    
    # Eliminate extra white spaces
    
    text <- tm_map(text, stripWhitespace)
    
    # Perform word stemming
    
    text  <- tm_map(text, stemDocument)
    
    dtm <- TermDocumentMatrix(text)
    
    m <- as.matrix(dtm)
    
    v <- sort(rowSums(m),decreasing=TRUE)
    
    d <- data.frame(stem = names(v),freq=v)
    
    freq <- findFreqTerms(dtm, lowfreq = (n*lowfreq_rel))
    
    res <- d[which(d$stem %in% freq),]
    
    row.names(res) <- NULL
    
    if(nrow(res) > 0) {
      
      var <- names(other_data[c])
      res$variable <- var
      list[[c]] <- res
      
    }
  }
  
  common_words <- do.call(rbind, list)
  
  common_words <- common_words %>% 
    filter(!is.na(freq) & freq > lowfreq_abs) %>%
    select(variable, stem, freq) %>%
    arrange(desc(variable, freq))
  
  if(nrow(common_words) == 0) {
    print("No common words identified, based on the minimum frequencies that were set")
  }
  
  return(common_words)
  
}

# identifies sentence variations that feature the common word stems idenfified through the "identify_common_words" function 
# requires outputs from "pivot_longer_other" and "identify_common_words" functions

identify_common_variations <- function(other_data_long, common_words) {
  
  require(tidyverse)
  
  vlist <- list()
  
  other_data_long$text <- str_to_lower(other_data_long$text)
  other_data_long$text <- gsub("[[:punct:]]","", other_data_long$text)
  other_data_long$text <- gsub("“","", other_data_long$text)
  
  
  for (i in 1:nrow(common_words)) {
    
    v <- other_data_long %>% filter(variable == common_words[i,"variable"] & 
                                      str_detect(text, common_words[i,"stem"])) %>% 
      mutate(stem = common_words[i,"stem"]) %>% 
      rename(variation = text)
    
    v <- v %>% group_by(variable, stem, variation) %>% 
      summarize(freq = n()) %>% 
      arrange(desc(freq))
    
    vlist[[common_words[i,"stem"]]] <- v
    
  }
  
  common_variations <- do.call(rbind, vlist)
  
  return(common_variations)
}
