rankhospital <- function(state, outcome, num = "best"){
  
  #read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  outcome_data[, 11] <- as.numeric(outcome_data[, 11])
  outcome_data[, 17] <- as.numeric(outcome_data[, 17])
  outcome_data[, 23] <- as.numeric(outcome_data[, 23])
  
  states <- unique(outcome_data$State)
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  if(!(state %in% states)){
    
    #message("invalid state")
    
    return("invalid state")
    
  }
  
  if(!(outcome %in% outcomes)){
    
    #message("Invlaid Outcome")
    
    return("invalid Oucome")
  }
  
  message("Firt cut pass")
  
  #Outcome data for selected state
  outcome_data_state <- outcome_data[outcome_data$State == state, ]
  
  #outcome data for selected state and outcome specified
  
  if(outcome == "heart attack"){
    
    #remove NA for given outcome specified
    outcome_data_state_ouctomespec <-  outcome_data_state[!is.na(outcome_data_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), ]
    hospital_outcome <- tapply(outcome_data_state_ouctomespec$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, outcome_data_state_ouctomespec$Hospital.Name, sum)
    val <- sort(hospital_outcome, decreasing = FALSE)
    
    
    
  }
  
  else if (outcome == "heart failure") {
    
    outcome_data_state_ouctomespec <-  outcome_data_state[!is.na(outcome_data_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure), ]
    hospital_outcome <- tapply(outcome_data_state_ouctomespec$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, outcome_data_state_ouctomespec$Hospital.Name, sum)
    val <- sort(hospital_outcome, decreasing = FALSE)
    
    
  }
  
  else if(outcome == "pneumonia"){
    
    outcome_data_state_ouctomespec <-  outcome_data_state[!is.na(outcome_data_state$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia), ]
    hospital_outcome <- tapply(outcome_data_state_ouctomespec$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, outcome_data_state_ouctomespec$Hospital.Name, sum)
    val <- sort(hospital_outcome, decreasing = FALSE)
    
  }
  
  if (num == "worst"){
    
    #position <- -1
    #return (names(val[length(val)]))
    return (names(val[val == val[length(val)]]))
  }
  
  if(is.numeric(num)){
    
    position <- num
  }
  
  if(num == "best"){
    
    position <- 1
  }
  
  names(val[val == val[position]])
  #names(val[position])
}