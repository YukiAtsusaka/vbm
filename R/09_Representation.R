################################################################################################
# 09_Representation.R
# Created by Yuki Atsusaka
# Since 12/19/2019
# Last updated 12/19/2019
# Aim: to see the representational gap before and after the VBM adoption in 2013
################################################################################################

################################################################################################
rm(list=ls())
library(tidyverse)
library(magrittr)

dat <- read_csv("Stack_Colorado_NC_2012_2016_Imputed.csv", col_types = cols(VoterID = col_character())) # PRIMARY POPULATION OF INTEREST
dat <- dat %>% 
       filter(is.na(female)==F & is.na(democrat)==F & is.na(age)==F & is.na(estrace)==F) %>%
       mutate(white=ifelse(estrace=="White",1,0),
              black=ifelse(estrace=="Black",1,0),
              hisp=ifelse(estrace=="Hispanic",1,0),
              asian=ifelse(estrace=="Asian",1,0),
              young=ifelse(age<35,1,0),
              middle=ifelse(age>=35 & age<65,1,0),
              old=ifelse(age>=65,1,0))
          
# SEE THE AGGREGTE TURNOUT
dat %>% group_by(State, Year) %>% summarise(Turnout = mean(Vote))
datCO <- dat %>% filter(Place==1) # Colorado Voters
datNC <- dat %>% filter(Place==0) # North Carolina Voters


vars <- c("voted2010", "white", "black", "hisp", "asian","female", "democrat", "young", "middle", "old")
#datCO %>% filter(Time==0) %>% summarize_at(vars, mean)           -> a
datCO %>% filter(Time==0 & Vote==1) %>% summarize_at(vars, mean) -> b
#datCO %>% filter(Time==1) %>% summarize_at(vars, mean)           -> c
datCO %>% filter(Time==1 & Vote==1) %>% summarize_at(vars, mean) -> d
bind <- bind_rows(b,d)


# COLORADO 
# bottom, left, top, and right. The default is c(5.1, 4.1, 4.1, 2.1).
nvec=c("2012", "2016")
cvec=c("gray", "gray30")
pdf("Representation.pdf", width=10, height=6)
par(mfrow=c(3,4), mar=c(3,3,4,1))
bind[,"voted2010"] %>% pull() %>% barplot(main="Frequent Voter", names.arg=nvec, col=cvec, ylim=c(0,1))
abline(h=bind[2,"voted2010"], col="firebrick4", lty=2)
bind[,"white"]    %>% pull() %>% barplot(main="White", names.arg=nvec, col=cvec, ylim=c(0,1))
abline(h=bind[2,"white"], col="firebrick4", lty=2)
bind[,"black"]    %>% pull() %>% barplot(main="Black", names.arg=nvec, col=cvec, ylim=c(0,1))
abline(h=bind[2,"black"], col="firebrick4", lty=2)
bind[,"hisp"]     %>% pull() %>% barplot(main="Hispanic", names.arg=nvec, col=cvec, ylim=c(0,1))
abline(h=bind[2,"hisp"], col="firebrick4", lty=2)
bind[,"asian"]    %>% pull() %>% barplot(main="Asian", names.arg=nvec, col=cvec, ylim=c(0,1))
abline(h=bind[2,"asian"], col="firebrick4", lty=2)
bind[,"female"]   %>% pull() %>% barplot(main="Female", names.arg=nvec, col=cvec, ylim=c(0,1))
abline(h=bind[2,"female"], col="firebrick4", lty=2)
bind[,"democrat"] %>% pull() %>% barplot(main="Democrat", names.arg=nvec, col=cvec, ylim=c(0,1))
abline(h=bind[2,"democrat"], col="firebrick4", lty=2)
bind[,"young"]    %>% pull() %>% barplot(main="Under 35", names.arg=nvec, col=cvec, ylim=c(0,1))
abline(h=bind[2,"young"], col="firebrick4", lty=2)
bind[,"middle"]   %>% pull() %>% barplot(main="35 to 65", names.arg=nvec, col=cvec, ylim=c(0,1))
abline(h=bind[2,"middle"], col="firebrick4", lty=2)
bind[,"old"]      %>% pull() %>% barplot(main="Over 65", names.arg=nvec, col=cvec, ylim=c(0,1))
abline(h=bind[2,"old"], col="firebrick4", lty=2)
dev.off()


# # NORTH CAROLINA
# vars <- c("female", "democrat", "white", "black", "hisp", "asian", "young", "middle", "old")
# datNC %>% filter(Time==0) %>% summarize_at(vars, mean)           -> o
# datNC %>% filter(Time==0 & Vote==1) %>% summarize_at(vars, mean) -> p
# datNC %>% filter(Time==1) %>% summarize_at(vars, mean)           -> q
# datNC %>% filter(Time==1 & Vote==1) %>% summarize_at(vars, mean) -> r
# bind2 <- bind_rows(o,p,q,r)
# 
# nvec=c("2012", "Voted", "2016", "Voted")
# cvec=c("gray", "gray", "gray30", "gray30")
# par(mfrow=c(3,3), mar=c(3,4,4,2))
# bind2[,"female"]   %>% pull() %>% barplot(main="Female", names.arg=nvec, col=cvec)
# bind2[,"democrat"] %>% pull() %>% barplot(main="Democrat", names.arg=nvec, col=cvec)
# bind2[,"white"]    %>% pull() %>% barplot(main="White", names.arg=nvec, col=cvec)
# bind2[,"black"]    %>% pull() %>% barplot(main="Black", names.arg=nvec, col=cvec)
# bind2[,"hisp"]     %>% pull() %>% barplot(main="Hispanic", names.arg=nvec, col=cvec)
# bind2[,"asian"]    %>% pull() %>% barplot(main="Asian", names.arg=nvec, col=cvec)
# bind2[,"young"]    %>% pull() %>% barplot(main="Under 35", names.arg=nvec, col=cvec)
# bind2[,"middle"]   %>% pull() %>% barplot(main="35 to 65", names.arg=nvec, col=cvec)
# bind2[,"old"]      %>% pull() %>% barplot(main="Over 65", names.arg=nvec, col=cvec)
# 

################################################################################################
# END OF THIS R SOURCE CODE
################################################################################################
  
