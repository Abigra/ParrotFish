#Parrot fish population function with Fishing and Noise
#Effort = E
#Noise = tau_r
#Carrying Capacity = K
#Growth Rate = r

E_Parrot_Function <- function(v_ex6,r,K,E,tau_r){
  
  y_i <- 1 
  
  df_PFISH_y_i <- data.frame(Year = y_i,
                             N_1=v_ex6[1], 
                             N_2=v_ex6[2],
                             N_3=v_ex6[3],
                             N_4=v_ex6[4]
  )
  ParrotFish_M <- rbind(
    c(0,0,df_PFISH_y_i$N_3*(exp(r*(1-df_PFISH_y_i$N_3/K))),0),
    c(0.55,0,0,0),
    c(0,0.55*(1-E),0.275*(1-E),0),
    c(0,0,0.275*(1-E),0)
  )
  
  while (y_i <= 400) {
    Parrot = ParrotFish_M%*%as.numeric(df_PFISH_y_i[y_i,2:5])
    y_i = y_i+1
    Parrot[2] <- ((rlnorm(1, meanlog=log(df_PFISH_y_i$N_3[y_i-1])+(r*(1-(df_PFISH_y_i$N_3[y_i-1])/K)), sdlog=tau_r)))
    df_PFISH_y_i <-rbind(df_PFISH_y_i, c(y_i,Parrot))
  }
  df_PFISH_y_i$Grazers <- (df_PFISH_y_i$N_2 + df_PFISH_y_i$N_3 + df_PFISH_y_i$N_4)
  
  return(df_PFISH_y_i)
}

