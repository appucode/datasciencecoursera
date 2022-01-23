rankall <- function(outcome, num = "best"){
  
  ret_val <- data.frame("hospital" = c(), "State" = c())

  
  #read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  outcome_data[, 11] <- as.numeric(outcome_data[, 11])
  outcome_data[, 17] <- as.numeric(outcome_data[, 17])
  outcome_data[, 23] <- as.numeric(outcome_data[, 23])
  
  states <- unique(outcome_data$State)
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  if(!(outcome %in% outcomes)){
    
    #message("Invlaid Outcome")
    
    return("invalid Oucome")
  }
  
  message("Firt cut pass")
  
  for (state in states){
    #Outcome data for selected state
    outcome_statedata <- outcome_data[outcome_data$State == state, ]
    
    if(outcome == "heart attack"){
      
      outcome_statedata_condition <- outcome_statedata[!is.na(outcome_statedata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), ]
      final_data <- outcome_statedata_condition[order(outcome_statedata_condition$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), ]
      if(num == "best"){
        position <- 1
        ret_val <- rbind(ret_val, c(final_data[position, 2], state))
      }
      
      
      if(is.numeric(num)){
        position <- num
        ret_val <- rbind(ret_val, c(final_data[position, 2], state))
      }
      
      if(num == "worst"){
        ret_val <- rbind(ret_val, c(final_data[nrow(final_data), 2], state))
      }
      
    }
    
    if(outcome == "heart failure"){
      
      outcome_statedata_condition <- outcome_statedata[!is.na(outcome_statedata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure), ]
      final_data <- outcome_statedata_condition[order(outcome_statedata_condition$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure), ]
      if(num == "best"){
        position <- 1
        ret_val <- rbind(ret_val, c(final_data[position, 2], state))
      }
      
      
      if(is.numeric(num)){
        position <- num
        ret_val <- rbind(ret_val, c(final_data[position, 2], state))
      }
      if(num == "worst"){
        ret_val <- rbind(ret_val, c(final_data[nrow(final_data), 2], state))
      }
    }
    
    if(outcome == "pneumonia"){
      
      outcome_statedata_condition <- outcome_statedata[!is.na(outcome_statedata$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia), ]
      final_data <- outcome_statedata_condition[order(outcome_statedata_condition$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia), ]
      if(num == "best"){
        position <- 1
        ret_val <- rbind(ret_val, c(final_data[position, 2], state))
      }
      
      
      if(is.numeric(num)){
        position <- num
        ret_val <- rbind(ret_val, c(final_data[position, 2], state))
      }
      
      if(num == "worst"){
        ret_val <- rbind(ret_val, c(final_data[nrow(final_data), 2], state))
      }
    }
    
    
  }
  
  names(ret_val) <- c("hospital", "state")
  ret_val
  
 
}