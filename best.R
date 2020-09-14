best<-function(state,outcome){
          
          outcome_list <- c('heart attack','heart failure','pneumonia')
          try(if(is.element(outcome,outcome_list)==FALSE) stop("Invalid Outcome name"))
          state_list <- c("AL","AK","AR","CA","AZ","CO","CT","DE","DC",
                          "FL","GA","HI","ID","IL","IN","IA","KS","KY",
                          "LA","ME","MD","MA","MI","MN","MS","MO","MT",
                          "NE","NV","NH","NJ","NM","NY","NC","ND","OH",
                          "OK","OR","PA","PR","RI","SC","SD","TN","TX",
                          "UT","VT","VA","WA","WV","WI","WY","TX")
          try(if(is.element(state,state_list)==FALSE) stop("Invalid State Name"))
          
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
          min_index <- which(outcome_cleaned==min(outcome_cleaned))
          best_hospital<-hospital_name_cleaned[min_index]
          best_hospital <- sort(best_hospital)
          
          
          return (best_hospital[1])
         
          
}