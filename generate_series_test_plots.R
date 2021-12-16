a <- 0.2
r<- 0.1
d <- .035
y<- .5
dt<- .01


test_1 <-generate_series(.4,.4,.21)
test_2<- generate_series(.1,.1,.21)
test_3<- generate_series(.4,.1,.21)
test_4<- generate_series(.1,.4,.21)
ggplot()+
  geom_point(data=test_1,aes(x=v_M_values1, y=v_C_values1, color=".4,.4"))+
  geom_point(data=test_2, aes(x=v_M_values1, y=v_C_values1, color=".1,.1"))+
  geom_point(data=test_3, aes(x=v_M_values1, y=v_C_values1, color=".1,.4"))+
  geom_point(data=test_4, aes(x=v_M_values1, y=v_C_values1, color=".4,.1"))
  