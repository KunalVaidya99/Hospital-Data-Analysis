hospital_data<-read.csv("outcome-of-care-measures.csv",colClas="character")
split_state <- split(hospital_data,hospital_data$State)

state_best<-function(state,outcome,number){
  
  
        
  
        state_list <- c("AL","AK","AR","CA","AZ","CO","CT","DE","DC",
                        "FL","GA","HI","ID","IL","IN","IA","KS","KY",
                        "LA","ME","MD","MA","MI","MN","MS","MO","MT",
                        "NE","NV","NH","NJ","NM","NY","NC","ND","OH",
                        "OK","OR","PA","PR","RI","SC","SD","TN","TX",
                        "UT","VT","VA","WA","WV","WI","WY","TX")
        
        
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



rankall<-function(outcome,num=1){
  
  outcome_list <- c('heart attack','heart failure','pneumonia')
  try(if(is.element(outcome,outcome_list)==FALSE) stop("Invalid Outcome name"))
  
  state_list <- c("AL","AK","AR","CA","AZ","CO","CT","DE","DC",
                  "FL","GA","HI","ID","IL","IN","IA","KS","KY",
                  "LA","ME","MD","MA","MI","MN","MS","MO","MT",
                  "NE","NV","NH","NJ","NM","NY","NC","ND","OH",
                  "OK","OR","PA","PR","RI","SC","SD","TN",
                  "UT","VT","VA","VI","WA","WV","WI","WY","TX")
  
  desiredlength = length(state_list)
  output <- data.frame(hospital<-character(desiredlength),state<-character<-character(desiredlength))
  
  
  j = 0
  for (i in state_list){
    j= j+1
    best_hosp <- state_best(i,outcome,num)
    output$hospital[j] = best_hosp
    output$state[j] = i
  }
  o = order(output$state)
  output$state = output$state[o]
  output$hospital = output$hospital[o] 
  return (output)
    
  
  

  
  
}