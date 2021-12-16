#functions for the single species model
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

#Determining yield from effort
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


#functions for the ecosystem based model

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

