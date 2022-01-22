pollutantmean <- function(directory, pollutant, id = 1:332){
  df_sum <- 0
  df_count <-0
  
  
  #File Name Generation

  for (i in id){
    
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
    
    # Mean value identification for files
    df <- read.csv(file.path(getwd(), directory, file_name), header = TRUE, sep = ",")
    df_sum <- df_sum + sum(df[[pollutant]], na.rm = TRUE)
    df_count <- df_count + sum(!is.na(df[[pollutant]]))

  }
  
  df_avg <- df_sum / df_count
  
  df_avg
  
  
}