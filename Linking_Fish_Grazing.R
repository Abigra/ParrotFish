#Created column called Grazers (addition of 3 age phases that graze)
#Averaged these values from year 100-150
#No fishing
#Number of fish to correlate with upper bound of grazing
#Hello

set.seed(5)

FISH <- E_Parrot_Function(v_ex6,0.71,1,0,0.1)

Avg_MaxFish <- mean(FISH$Grazers[100:150])


#Minimum grazing (lots of fishing)

set.seed(7)

FISH <- E_Parrot_Function(v_ex6,0.71,1,0.3,0.1)

Avg_MinFish <- mean(FISH$Grazers[100:150])



#Line for Grazing/Fish Number

GrazeMax <- 0.385
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
  geom_point(aes(x= 1.59, y=0.385),shape= 8, color="blue", size=8)+
  labs(x="Population Size", y="Grazing Rate (% algae per time step)")

