#figures 
#yield from effort
plot2<- data.frame(e, Effort_Frame$Avg_Yield)
yield_from_effort<-ggplot(data= plot2, aes(x=e, y=Effort_Frame$Avg_Yield))+
  geom_line(color="pink")+
  geom_point(aes(x=0.16, y=0.15572774),shape= 8, color="blue", size=8)+
  labs(x="Effort", y="Average Yield (parrotfish)")
#grazing by fishing
plot<- data.frame(Fish_test,Grazing_Test)
fishing_vs_grazing<-ggplot(data= plot, aes(x= Fish_test, y=Grazing_Test))+
  geom_line(color="pink")+
  geom_point(aes(x= 1.509, y=0.4),shape= 8, color="blue", size=8)+
  labs(x="Average Yield (parrotfish)", y="Grazing Rate (% algae per time step)")
#proportions of coral stable states by G values
coral_stable_states_by_g<-ggplot(data=proportions_df,aes(x=g, y=proportions),color="g")+
  geom_point()+
  labs(title="Proportion of starting conditions which result in coral stable states by grazing value", x="Grazing Rate", y="Proportion in Coral Stable State", fill="Coral Stable State")+
  theme_bw()
#comparison of macroalgal vs coraline stable states by g values
stable_states<-ggplot(data=simulated_g_data,aes(x=g, fill=coral_state))+
  geom_bar()+
  xlim(0.15, 0.5)+
  labs(title="Number of starting conditions which result in coral stable states by grazing value", x="Grazing Rate", y="Number in Coral Stable State", fill="Coral Stable State")+
  theme_bw()+
  scale_fill_discrete(name="Stable State", labels=c("Macroalgal State", "Coral State"))
#figure containing the trajectories for 4 different tested grazing values
trajectories_figure <- ggarrange(plot3,plot1,plot4,plot2)
trajectories_figure
#figure for resilience vs yield
Resilience_Yield_Figure<-ggplot(data=Resilience_Frame, aes(x=Avg_Yield, y=Resilience))+
  geom_point()+
  geom_vline(xintercept=c(0.1557), linetype="dotted")+
  labs(title="Yield vs Resilience of Ecosystem", x="Average Yield" )+
  theme_bw()

#figure for population simulation
ggplot(data=FISH, aes(x=Year, y=N_2)) +
  geom_line(color="red") +
  geom_line(aes(x=Year, y=N_3), color="yellow")+
  geom_line(aes(x=Year, y=N_4,), color="green")+
  #geom_line(aes(x=Year, y=Yield_A,), color="purple")+
  labs(x= "Year", y= "Population Distribution")