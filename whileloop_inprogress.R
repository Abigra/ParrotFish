#converting for loop to while loop

generate_series <- function(C, M, g){
  v_M_values1 <- vector()
  v_C_values1 <- vector()
  T_<- 1-M-C
  v_M_values1[1] <- M 
  v_C_values1[1]<- C
  index_time=2
  delta_M <-1
  delta_C <-1
  while(sqrt(delta_M^2+delta_C^2) >.000000000000001){
    current_M_value <- v_M_values1[index_time-1]
    current_C_value <- v_C_values1[index_time-1]
    v_M_values1[index_time] <- current_M_value+(dt*F_dMdt(a,M,C,g,T_,y))
    v_C_values1[index_time] <- current_C_value+ (dt*F_dCdt(r,T_,C,d,a,M))
    delta_M<- current_M_value-M
    delta_C<- current_C_value-C
    M <- current_M_value
    C <- current_C_value
    T_<- 1-M-C
    single_series_df <- data.frame(v_M_values1,v_C_values1)
    index_time<- index_time+1
  }
  return(single_series_df)
}

generate_series(.6,.1,.3)
