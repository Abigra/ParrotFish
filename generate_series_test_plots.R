library(ggpubr)
a <- 0.2
r<- 0.1
d <- .035
y<- .5
dt<- .01

g=.21
  
test_1 <-generate_series(.4,.4,g)
test_2<- generate_series(.1,.1,g)
test_3<- generate_series(.4,.1,g)
test_4<- generate_series(.1,.4,g)

plot4<- ggplot()+
  geom_point(data=test_1,aes(x=v_M_values1, y=v_C_values1, color=".4,.4"))+
  geom_point(data=test_2, aes(x=v_M_values1, y=v_C_values1, color=".1,.1"))+
  geom_point(data=test_3, aes(x=v_M_values1, y=v_C_values1, color=".1,.4"))+
  geom_point(data=test_4, aes(x=v_M_values1, y=v_C_values1, color=".4,.1"))+
  labs(x="Macroalgae Cover", y= "Coral Cover", title=paste(g))

plot1
plot2
plot3 
plot4

save(plot1,plot2,plot3,plot4, file="simulated_test_trajectories.Rdata")

trajectories_figure<- par(mfrow=c(2,2))
plot1
plot2
plot3
plot4
trajectories_figure <- ggarrange(plot3,plot1,plot4,plot2)

trajectories_figure
