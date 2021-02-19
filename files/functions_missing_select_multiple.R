
# identifies missing string in "select multiple" character variable, 
# considering the associated value of binary variable
# requires vectors with the names of the SM character and binary variables

find_missing_sm_string <- function(df, 
                                   vars_sm_char, 
                                   vars_sm_bin) {
  require(tidyverse)
  
  df <- df %>%
    select(uuid, all_of(vars_sm_char), all_of(vars_sm_bin))
  
  d <- data.frame()
  
  f = 1
  
  for (c in 1:ncol(df)) {
    
    if(colnames(df[,c]) %in% all_of(vars_sm_bin)) {
      
      for (i in 1:nrow(df)) {
        
        if(!is.na(df[i,c]) &&
           df[i,c] == 1 &&
           !(grepl(paste0(" ",gsub("\\w+\\.", "", colnames(df[i,c]))), 
                   df[i,gsub("\\.\\w+", "", colnames(df[i,c]))]) ||
             grepl(paste0("^",gsub("\\w+\\.", "", colnames(df[i,c]))), 
                   df[i,gsub("\\.\\w+", "", colnames(df[i,c]))]))) {
          
          d[f, "uuid"] <- df[i,"uuid"]
          d[f, "var_char"] <- gsub("\\.\\w+", "", colnames(df[i,c]))
          d[f, "var_bin"] <- colnames(df[i,c])
          d[f, "value_char"] <- df[i,gsub("\\.\\w+", "", colnames(df[i,c]))]
          d[f, "value_bin"] <- df[i,colnames(df[i,c])]
          d[f, "check"] <- "binary_1_but_missing_sm_string"
          f = f + 1
        }
      }
    }
  }
  
  return(d)
  
}

# identifies missing 1 in "select multiple" binary variable, 
# considering the associated string of character variable
# requires vectors with the names of the SM character and binary variables

find_missing_sm_1 <- function(df, 
                                vars_sm_char, 
                                vars_sm_bin) {
  
  require(tidyverse)
  
  df <- df %>%
    select(uuid, all_of(vars_sm_char), all_of(vars_sm_bin))
  
  d <- data.frame()
  
  f = 1
  
  for (c in 1:ncol(df)) {
    
    if(colnames(df[,c]) %in% all_of(vars_sm_bin)) {
      
      for (i in 1:nrow(df)) {
        
        if((is.na(df[i,c]) ||
            df[i,c] == 0 ||
            df[i,c] == "") &&
           (grepl(paste0(" ",gsub("\\w+\\.", "", colnames(df[i,c]))), 
                  df[i,gsub("\\.\\w+", "", colnames(df[i,c]))]) ||
            grepl(paste0("^",gsub("\\w+\\.", "", colnames(df[i,c]))), 
                  df[i,gsub("\\.\\w+", "", colnames(df[i,c]))]))) {
          
          d[f, "uuid"] <- df[i,"uuid"]
          d[f, "var_char"] <- gsub("\\.\\w+", "", colnames(df[i,c]))
          d[f, "var_bin"] <- colnames(df[i,c])
          d[f, "value_char"] <- df[i,gsub("\\.\\w+", "", colnames(df[i,c]))]
          d[f, "value_bin"] <- df[i,colnames(df[i,c])]
          d[f, "check"] <- "sm_string_but_missing_binary_1"
          f = f + 1
        }
      }
    }
  }
  
  return(d)
  
}
