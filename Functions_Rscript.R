

# Create the functions based on 
#Mumby et. al (Nature, 2007)
#    F_dMdt (macroalgal growth rate function) and 
#    F_dCdt (coral growth rate)


F_dMdt <- function(a, M, C, g, T_, y){
  dMdt <- (a*M*C)-((g*(M/(M+T_))))+(y*M*T_)
  return(dMdt)
}


F_dCdt <- function(r,T_, C, d, a, M){
  dCdt <- (r*T_*C)-(d*C)-(a*M*C)
  return(dCdt)
}


#create a function that generates a single series of start values
#    generate_series

generate_series <- function(C, M, g){
  T_<- 1-M-C
  v_M_values1[1] <- M 
  v_C_values1[1]<- C
  for(index_time in 2:length(v_M_values1)){
    current_M_value <- v_M_values1[index_time-1]
    current_C_value <- v_C_values1[index_time-1]
    v_M_values1[index_time] <- current_M_value+(dt*F_dMdt(a,M,C,g,T_,y))
    v_C_values1[index_time] <- current_C_value+ (dt*F_dCdt(r,T_,C,d,a,M))
    M <- current_M_value
    C <- current_C_value
    T_<- 1-M-C
    single_series_df <- data.frame(v_M_values1,v_C_values1)
  }
  return(single_series_df)
}

#create a function that loops through the start values for 1 value of g
#    f_start_values

f_start_values <- function(g){
  
  for(index_start in 1:nrow(paired_values) ){
    M<- paired_values[index_start,1]
    C<- paired_values[index_start,2]
    stored_values<-generate_series(C,M,g)
    #issue with how the data is being stored after each start value has been tested
    paired_values$Mfinal[index_start]<- stored_values[1000,1]
    paired_values$Cfinal[index_start]<- stored_values[1000,2]
  }
  
  return(paired_values)
}


