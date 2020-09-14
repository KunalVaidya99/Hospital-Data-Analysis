rankhospital<-function(state,outcome,number){
  
  outcome_list <- c('heart attack','heart failure','pneumonia')
  if(is.element(outcome,outcome_list)==FALSE) stop("Invalid Outcome name")
  state_list <- c("AL","AK","AR","CA","AZ","CO","CT","DE","DC",
                  "FL","GA","HI","ID","IL","IN","IA","KS","KY",
                  "LA","ME","MD","MA","MI","MN","MS","MO","MT",
                  "NE","NV","NH","NJ","NM","NY","NC","ND","OH",
                  "OK","OR","PA","PR","RI","SC","SD","TN",
                  "UT","VT","VA","WA","WV","WI","WY","TX")
  if(is.element(state,state_list)==FALSE) stop("Invalid State Name")
  
  hospital_data<-read.csv("outcome-of-care-measures.csv",colClas="character")
  split_state <- split(hospital_data,hospital_data$State)
  #print(split_state)
  
  
  state_data<-split_state[state]
  state_data<-do.call(rbind.data.frame,state_data)
  
  #print(is.element(outcome,outcome_list))
  
  if(outcome == 'heart attack'){
    outcome_data <- as.numeric(state_data[,11]) 
  }
  if(outcome == 'heart failure'){
    outcome_data <- as.numeric(state_data[,17])                          
  }
  if(outcome == 'pneumonia'){
    outcome_data <- as.numeric(state_data[,23])                          
  }
  #print(outcome_data)
  bad = is.na(outcome_data)
  #print(outcome_data)
  hospital_name = state_data[,2]
  hospital_name_cleaned = hospital_name[!bad]
  outcome_cleaned <- outcome_data[!bad]
  sorted_indices <- order(outcome_cleaned,hospital_name_cleaned)
  outcome_cleaned <- sort(outcome_cleaned)
  #print(sorted_indices)
  #min_index <- outcome_cleaned[number]
  hospital_name_cleaned <- (hospital_name_cleaned[sorted_indices])
  #hospital_name_cleaned <- sort(hospital_name_cleaned)
  len <- length(hospital_name_cleaned)
  if(number =="worst"){
      
        best_hospital <- tail(hospital_name_cleaned,n=1)
  }
  else{
    best_hospital<-hospital_name_cleaned[number]
    
  }
  
  
  
  
  return (best_hospital)
  
  
}