complete <- function(directory, id = 1: 332){
  
  #Empty return dataframe creation
  ret_df <- data.frame(matrix(nrow = 0, ncol = 2))
  #colnames(ret_df) <- c("id", "nobs")
  
  
  
  #for each file id
  
  for (i in id){
    
    #File Name Generation
    
    if(i < 10){ 
      file_name <- paste("00", i, ".csv", sep = "")
    }
    
    else if(i < 100){
      file_name <- paste("0", i, ".csv", sep = "")
    }
    
    else{
      file_name <- paste(i, ".csv", sep = "")
    }
    
    #print(file_name)
    
    
    #File content reading    
    df <- read.csv(file.path(getwd(), directory, file_name), header = TRUE, sep = ",")
    
    #creating row details
    entry <- c(i, sum(!is.na(df[["sulfate"]]) & !is.na(df[["nitrate"]])))
    
    #append row to data frame
    ret_df <- rbind (ret_df, entry)
    
  }
  
  colnames(ret_df) <- c("id", "nobs")
  
  ret_df
  
}

