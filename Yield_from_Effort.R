# Finding Yield from Effort

#Average Yield vs. Effort
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


plot2<- data.frame(e, Effort_Frame$Avg_Yield)
ggplot(data= plot2, aes(x=e, y=Effort_Frame$Avg_Yield))+
  geom_line(color="pink")+
  geom_point(aes(x=0.16, y=0.15572774),shape= 8, color="blue", size=8)+
  labs(x="Effort", y="Average Yield (parrotfish)")



