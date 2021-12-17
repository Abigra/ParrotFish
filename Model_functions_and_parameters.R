###single species model functions and calculations

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
  df_PFISH_y_i$Yield_IP <- E*(df_PFISH_y_i$N_2)/(1-E)
  df_PFISH_y_i$Yield_A <- E*(df_PFISH_y_i$N_3)/(1-E)
  df_PFISH_y_i$Yield_TP  <- E*(df_PFISH_y_i$N_4)/(1-E)
  df_PFISH_y_i$Total_Yield <- (df_PFISH_y_i$Yield_IP + df_PFISH_y_i$Yield_A + df_PFISH_y_i$Yield_TP)
  
  return(df_PFISH_y_i)
}

v_ex6 <- matrix(c(0.5,0.5,0.5,0.5), nrow = 4, byrow = TRUE)


FISH <- E_Parrot_Function(v_ex6,0.71,1,0.1,0.1)


ggplot(data=FISH, aes(x=Year, y=N_2)) +
  geom_line(color="red") +
  geom_line(aes(x=Year, y=N_3), color="yellow")+
  geom_line(aes(x=Year, y=N_4,), color="green")+
  #geom_line(aes(x=Year, y=Yield_A,), color="purple")+
  labs(x= "Year", y= "Population Distribution")

#Created column called Grazers (addition of 3 age phases that graze)
#Averaged these values from year 100-150
#No fishing
#Number of fish to correlate with upper bound of grazing

set.seed(5)

FISH <- E_Parrot_Function(v_ex6,0.71,1,0,0.1)

Avg_MaxFish <- mean(FISH$Grazers[100:150])


#Minimum grazing (lots of fishing)

set.seed(7)

FISH <- E_Parrot_Function(v_ex6,0.71,1,0.3,0.1)

Avg_MinFish <- mean(FISH$Grazers[100:150])



#Line for Grazing/Fish Number

GrazeMax <- 0.42
GrazeMin <- 0.1

m <- (GrazeMax - GrazeMin)/(Avg_MaxFish - Avg_MinFish)
B <- (-m*(Avg_MaxFish) +GrazeMax)

GrazeLine <- function(fish_number){
  Grazing <- m*(fish_number) + B
  
  return(Grazing)
}

Fish_test <- (seq(0,2, by=0.1))

Grazing_Test <- GrazeLine(Fish_test)


plot<- data.frame(Fish_test,Grazing_Test)
ggplot(data= plot, aes(x= Fish_test, y=Grazing_Test))+
  geom_line(color="pink")+
  geom_point(aes(x= 1.59, y=0.21),shape= 8, color="blue", size=8)+
  labs(x="Average Yield (parrotfish)", y="Grazing Rate (% algae per time step)")

e <- (seq(0,0.3, by=0.01)) 
rho <- 1

Effort_Frame <- data.frame(Effort = e,
                           Avg_Yield= NA
)

for (iter in e){
  TEST <- E_Parrot_Function(v_ex6,0.71,1,iter,0)
  Effort_Frame$Avg_Yield[rho] <- sum(TEST$Total_Yield[100:150]/51)
  rho <- rho +1
}
plot(e,Effort_Frame$Avg_Yield )

which.max(Effort_Frame$Avg_Yield)



###functions for the ecosystem based model

a <- 0.2
r<- 0.1
d <-0.035
y<- 0.5
dt<- .01

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
  v_M_values1 <- vector()
  v_C_values1 <- vector()
  v_M_values1[1] <-M 
  v_C_values1[1]<- C
  T_<- 1-M-C
  index_time=2
  delta_M <-1
  delta_C <-1
  while(sqrt(delta_M^2+delta_C^2) >.00000001){
    M <- v_M_values1[index_time-1]
    C <- v_C_values1[index_time-1]
    T_<- 1-M-C
    newM<- M+(dt*F_dMdt(a,M,C,g,T_,y))
    newC <- C+ (dt*F_dCdt(r,T_,C,d,a,M))
    v_M_values1[index_time] <- newM
    v_C_values1[index_time]<- newC
    delta_M<- newM-M
    delta_C<- newC-C
    index_time<- index_time+1
  }
  single_series_df <- data.frame(v_M_values1,v_C_values1)
  return(single_series_df)
}

#create a function that loops through the start values for 1 value of g
#    f_start_values

list_M_values<- seq(0,1,by=.05)
list_C_values <- seq(0,1,by=.05)
paired_values<-expand.grid(list_M_values,list_C_values)
paired_values<-subset(paired_values, Var1+Var2<1)

f_start_values <- function(g){
  
  for(index_start in 1:nrow(paired_values) ){
    M<- paired_values[index_start,1]
    C<- paired_values[index_start,2]
    stored_values<-generate_series(C,M,g)
    paired_values$Mfinal[index_start]<- stored_values[nrow(stored_values),1]
    paired_values$Cfinal[index_start]<- stored_values[nrow(stored_values),2]
  }
  
  return(paired_values)
}

#paired_values$coral_state<- if(paired_values$Mfinal<.01){TRUE}else{FALSE}

