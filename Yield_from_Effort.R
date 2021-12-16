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



