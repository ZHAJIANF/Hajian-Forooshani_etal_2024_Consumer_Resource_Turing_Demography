rm(list=ls())
data <- read.csv("Azteca_DSP_Phorid_dynamics_survey_data.csv")
head(data)


library(brms)
library(performance)

#### Final models 
#### Number of Phorids model 
logged.num.phorid.linear.mod <- brm(log(num_phorids) ~ nest_age + circ + (1|hectare) , data= data) 
logged.num.phorid.linear.mod
check_model(logged.num.phorid.linear.mod)
plot(logged.num.phorid.linear.mod)
plot(conditional_effects(logged.num.phorid.linear.mod),points=T)
pp_check(logged.num.phorid.linear.mod,ndraws=100)

### phorid arrival time model 
logged.arrival.phorid.linear.mod <- brm(log(arrival_time) ~ nest_age + circ + (1|hectare), data= data)
logged.arrival.phorid.linear.mod
plot(logged.arrival.phorid.linear.mod)
plot(conditional_effects(logged.arrival.phorid.linear.mod),points=T)
pp_check(logged.arrival.phorid.linear.mod,ndraws=100)
check_model(logged.arrival.phorid.linear.mod)

### phorid arrival time model 
logged.attack.phorid.linear.model <- brm(log(attack_time)  ~ nest_age + circ + (1|hectare), data= data)
logged.attack.phorid.linear.model
plot(logged.attack.phorid.linear.model)
plot(conditional_effects(logged.attack.phorid.linear.model),points=T)
pp_check(logged.attack.phorid.linear.model,ndraws=100)
check_model(logged.attack.phorid.linear.model)

head(data)
### phorid num attacks model 
logged.num.attacks.phorid.linear.model <- brm(log(num_attacks+1)  ~ nest_age + circ + (1|hectare), data= data)
logged.num.attacks.phorid.linear.model
plot(logged.attack.phorid.linear.model)
plot(conditional_effects(logged.num.attacks.phorid.linear.model),points=T)
pp_check(logged.num.attacks.phorid.linear.model,ndraws=100)
check_model(logged.num.attacks.phorid.linear.model)




### Figures for main text 

#### Figure with nest age 
dev.new(height=3,width=9.5)
par(mfrow=c(1,3),mai=c(0.6,0.6,0.1,0.1))
pch.val <- 19

plot(log(data$arrival_time) ~ jitter(data$nest_age,amount=0.4),data=data,xlab="",ylab="",yaxt="n",pch=pch.val)
axis(2,las=2)
mtext("Ant nest age",side=1,line=2.2,cex=0.8)
mtext("ln(parasitoid arrival time (s))",side=2,line=2.5,cex=0.8)
abline(lm(log(data$arrival_time) ~ data$nest_age),lty=3,lwd=2)

plot(log(data$num_phorids) ~ jitter(data$nest_age,amount=0.4),data=data,xlab="",ylab="",yaxt="n",pch=pch.val)
axis(2,las=2)
mtext("Ant nest age",side=1,line=2.2,cex=0.8)
mtext("ln(# of parasitoids)",side=2,line=2.5,cex=0.8)
abline(lm(log(data$num_phorids) ~ data$nest_age),lty=3,lwd=2)

plot(log(data$attack_time) ~ jitter(data$nest_age,amount=0.4),data=data,xlab="",ylab="",yaxt="n",pch=pch.val)
axis(2,las=2)
mtext("Ant nest age",side=1,line=2.2,cex=0.8)
mtext("ln(parasitoid attack duration (s))",side=2,line=2.5,cex=0.8)
abline(lm(log(data$attack_time) ~ data$nest_age),lty=3,lwd=2)












### Full figures for supp material 

#### Figure with nest age 
dev.new(height=3,width=12)
par(mfrow=c(1,4),mai=c(0.6,0.6,0.1,0.1))
pch.val <- 19

plot(log(data$arrival_time) ~ jitter(data$nest_age,amount=0.4),data=data,xlab="",ylab="",yaxt="n",pch=pch.val)
axis(2,las=2)
mtext("Ant nest age",side=1,line=2.2,cex=0.8)
mtext("ln(parasitoid arrival time (s))",side=2,line=2.5,cex=0.8)
abline(lm(log(data$arrival_time) ~ data$nest_age),lty=3,lwd=2)

plot(log(data$num_phorids) ~ jitter(data$nest_age,amount=0.4),data=data,xlab="",ylab="",yaxt="n",pch=pch.val)
axis(2,las=2)
mtext("Ant nest age",side=1,line=2.2,cex=0.8)
mtext("ln(# of parasitoids)",side=2,line=2.5,cex=0.8)
abline(lm(log(data$num_phorids) ~ data$nest_age),lty=3,lwd=2)

plot(log(data$attack_time) ~ jitter(data$nest_age,amount=0.4),data=data,xlab="",ylab="",yaxt="n",pch=pch.val)
axis(2,las=2)
mtext("Ant nest age",side=1,line=2.2,cex=0.8)
mtext("ln(parasitoid attack duration (s))",side=2,line=2.5,cex=0.8)
abline(lm(log(data$attack_time) ~ data$nest_age),lty=3,lwd=2)

plot(log(data$num_attacks+1) ~ jitter(data$nest_age,amount=0.4),data=data,xlab="",ylab="",yaxt="n",pch=pch.val)
axis(2,las=2)
mtext("Ant nest age",side=1,line=2.2,cex=0.8)
mtext("ln(# parasitoid attacks)",side=2,line=2.5,cex=0.8)
abline(lm(log(data$num_attacks+1) ~ data$nest_age),lty=3,lwd=2)

#### figure with tree circumference 

head(data)
dev.new(height=3,width=12)
par(mfrow=c(1,4),mai=c(0.6,0.6,0.1,0.1))
pch.val <- 19

plot(log(data$arrival_time) ~ jitter(data$circ,amount=0.1),data=data,xlab="",ylab="",yaxt="n",pch=pch.val,cex=(data$nest_age*0.2))
axis(2,las=2)
mtext("Tree circumference",side=1,line=2.2,cex=0.8)
mtext("ln(parasitoid arrival time (s))",side=2,line=2.5,cex=0.8)
abline(lm(log(data$arrival_time) ~ data$circ),lty=3,lwd=2)

plot(log(data$num_phorids) ~ jitter(data$circ,amount=0.1),data=data,xlab="",ylab="",yaxt="n",pch=pch.val,cex=(data$nest_age*0.2))
axis(2,las=2)
mtext("Tree circumference",side=1,line=2.2,cex=0.8)
mtext("ln(# of parasitoids)",side=2,line=2.5,cex=0.8)
abline(lm(log(data$num_phorids) ~ data$circ),lty=3,lwd=2)


plot(log(data$attack_time) ~ jitter(data$circ,amount=0.1),data=data,xlab="",ylab="",yaxt="n",pch=pch.val,cex=(data$nest_age*0.2))
axis(2,las=2)
mtext("Tree circumference",side=1,line=2.2,cex=0.8)
mtext("ln(parasitoid attack duration (s))",side=2,line=2.5,cex=0.8)
abline(lm(log(data$attack_time) ~ data$circ),lty=3,lwd=2)


plot((data$num_attacks) ~ jitter(data$circ,amount=0.1),data=data,xlab="",ylab="",yaxt="n",pch=pch.val,cex=(data$nest_age*0.2))
axis(2,las=2)
mtext("Tree circumference",side=1,line=2.2,cex=0.8)
mtext("ln(# parasitoid attacks)",side=2,line=2.5,cex=0.8)
abline(lm((data$num_attacks) ~ data$circ),lty=3,lwd=2)
mod <- lm((data$num_attacks) ~ data$circ)
summary(mod)


