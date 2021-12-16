#figures 
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
