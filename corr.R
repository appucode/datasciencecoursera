corr <- function(directory, threshold = 0){
  ret_vect <- c()
  
  for (i in dir(directory)){
    #print(i)
    df <- read.csv(file.path(getwd(), directory, i), header = TRUE, sep = ",")
    if(sum(!is.na(df[["sulfate"]]) & !is.na(df[["nitrate"]])) > threshold){
      
      ret_vect <- c(ret_vect, cor(df[["sulfate"]], df[["nitrate"]], use = "pairwise.complete.obs"))
    }
    
    
  }
  
  ret_vect
  
  
}