################################################################################################
# DID_BVMEffect.R
# Created by Yuki Atsusaka
# Since 8/27/2019
# Last updated 8/30/2019
# Aim: to use the difference-in-differences estimator for the ATE of the VBM policy in Colorado
################################################################################################

################################################################################################
# (1) SIMPLE RANDOM SAMPLING
rm(list=ls())
library(tidyverse)

dat <- read_csv("Stack_Colorado_NC_2012_2016_imputed.csv")

##############################################################################################
# TURNOUTS ACROSS STATES AND YEARS
##############################################################################################

dat %>% group_by(State, Year) %>% summarise(Turnout = mean(Vote))
dat %>% filter(voted2010==1) %>% group_by(State, Year) %>% summarise(Turnout = mean(Vote))
dat %>% filter(voted2010==0) %>% group_by(State, Year) %>% summarise(Turnout = mean(Vote))
dat %>% filter(estrace=="White") %>% group_by(State, Year) %>% summarise(Turnout = mean(Vote))
dat %>% filter(estrace=="Black") %>% group_by(State, Year) %>% summarise(Turnout = mean(Vote))
dat %>% filter(estrace=="Hispanic") %>% group_by(State, Year) %>% summarise(Turnout = mean(Vote))
dat %>% filter(estrace=="Asian") %>% group_by(State, Year) %>% summarise(Turnout = mean(Vote))
dat %>% filter(democrat==1) %>% group_by(State, Year) %>% summarise(Turnout = mean(Vote))
dat %>% filter(democrat==0) %>% group_by(State, Year) %>% summarise(Turnout = mean(Vote))
dat %>% filter(female==1) %>% group_by(State, Year) %>% summarise(Turnout = mean(Vote))
dat %>% filter(female==0) %>% group_by(State, Year) %>% summarise(Turnout = mean(Vote))
dat %>% filter(age<35) %>% group_by(State, Year) %>% summarise(Turnout = mean(Vote))
dat %>% filter(age>= 35 & age < 65) %>% group_by(State, Year) %>% summarise(Turnout = mean(Vote))
dat %>% filter(age>= 65) %>% group_by(State, Year) %>% summarise(Turnout = mean(Vote))



##############################################################################################
# Checking the Overlap in the Propensity Score
################################################################################################

rm(list=ls())
library(tidyverse)

dat <- read_csv("Stack_Colorado_NC_2012_2016_imputed.csv")

m <- glm(Place ~ as.factor(female)+as.factor(democrat)
                 +as.factor(estrace)+age+voted2010, dat, family="binomial")
ps_t <- predict(m, dat[dat$Place==1,], type="response")
ps_c <- predict(m, dat[dat$Place==0,], type="response")
rm(dat,m)


match.dat <- read_csv("Stack_Colorado_NC_2012_2016_Matched.csv")

m <- glm(Place ~ as.factor(female)+as.factor(democrat)
                 +as.factor(estrace)+age+voted2010, match.dat, family="binomial", weights=weights)
ps_t2 <- predict(m, match.dat[match.dat$Place==1,], type="response")
ps_c2 <- predict(m, match.dat[match.dat$Place==0,], type="response")
rm(match.dat,m)



pdf("PropensityScore.pdf", width=10, height=4)
par(mfrow=c(1,2))
plot(density(ps_c), type="n", xlab="Propensity Score", main="Before Matching", xlim=c(0,1))
lines(density(ps_t), col="firebrick4", lwd=2)
lines(density(ps_c), col="gray50", lty=2, lwd=2)
legend("topright", col = c("firebrick4", "gray50"), lty = c(1, 2), lwd=c(2,2), 
       cex = 1.1, bty = "n",
       legend = c("Colorado", 
                  "North Carolina"))


plot(density(ps_t2), type="n", xlab="Propensity Score", main="After Matching")
lines(density(ps_t2), col="firebrick4", lwd=2)
lines(density(ps_c2), col="gray50", lty=2, lwd=2)
legend("topright", col = c("firebrick4", "gray50"), lty = c(1, 2), lwd=c(2,2), 
       cex = 1.1, bty = "n",
       legend = c("Colorado", 
                  "North Carolina"))

dev.off()





################################################################################################
# END OF THIS R SOURCE CODE
################################################################################################
  
