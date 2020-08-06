################################################################################################
# NewMexico.R
# Created by Yuki Atsusaka
# Since 8/5/2020
# Last updated 8/5/2020
# Aim: to use the difference-in-differences estimator for the ATE of the VBM policy in Colorado
################################################################################################

################################################################################################
# (1) RACE CODING FOR NEW MEXICO DATA
################################################################################################
rm(list=ls())
library(tidyverse)
library(haven)
library(wru)

setwd("C:/Users/YUKI/Box/FromLaptop/Project/03_ColoradoVBM_BOB/VBM_analysis")
# dat <- read_dta("new mexico voter file 2017.dta")
# 
# dat2 <- dat %>% mutate(surname = text_name_last,
#                        voterID = text_registrant_id) %>%
#         dplyr::select(surname, voterID)
# write_csv(dat2, "NewMexico_surname.csv")


df = read_csv("NewMexico_surname.csv")
df$surname = gsub("[\U4E00-\U9FFF\U3000-\U303F]", "", df$surname) # Drop Error-based Chinese Letter

############################################################################
# PREDICT RACE BASED ON SURNAME-ONLY METHOD
out <- predict_race(df, surname.year=2010, surname.only=T) 
############################################################################

 est.c <- out[, 3:7] # ONLY EXTRACT RACE PROBABILITIES TO SPEED UP ALGORITHM
 est.c$highest <- as.numeric(apply(est.c, 1, which.max)) # WHICH COLUMN HAS THE HIGHEST PROBABILITY?
 est.c$race <- ifelse(est.c$highest==1, "White", NA)     # ASSIGN MOST LIKELY RACE
 est.c$race[is.na(est.c$race)==T] <- ifelse(est.c$highest[is.na(est.c$race)==T]==2, "Black", NA)
 est.c$race[is.na(est.c$race)==T] <- ifelse(est.c$highest[is.na(est.c$race)==T]==3, "Hispanic", NA)
 est.c$race[is.na(est.c$race)==T] <- ifelse(est.c$highest[is.na(est.c$race)==T]==4, "Asian", NA)
 est.c$race[is.na(est.c$race)==T] <- ifelse(est.c$highest[is.na(est.c$race)==T]==5, "Others", NA)
 
 hist(est.c$highest, main="Histogram of Estimated Race", xlab="1=White, 2=Black, 3=Hispanic, 4=Asian, 5=Others")  
 table(est.c$race)/dim(df)[1]
 
# CHECK THE DISTRIBUTION
# THE PICTURE LOOKS CONSISTENT WITH COLORADO DEMOGRAPHICS
 df$est.race <- est.c$race # FINALLY, ADD ESTIMATED RACE TO THE ORIGINAL DATA FRAME
 head(df) # CHECK THE LAST COLUMN 
 mean(is.na(df$est.race)==F) # Everyone was coded

 df <- df %>% dplyr::select(voterID, est.race)
 
write_csv(df, "NewMexico_Race.csv")

################################################################################################
# (2) DATA BUILDING FOR NEW MEXICO
################################################################################################
rm(list=ls())
library(tidyverse)
library(haven)
library(lubridate)
setwd("C:/Users/YUKI/Box/FromLaptop/Project/03_ColoradoVBM_BOB/VBM_analysis")


estrace <- read_csv("NewMexico_Race.csv")
dat <- read_dta("new mexico voter file 2017.dta")

dat2 <- dat %>% mutate(voterID = text_registrant_id,
                       female =  ifelse(cde_gender=="F",1,0),
                       democrat = ifelse(desc_party=="DEMOCRAT",1,0),
                       birthyear = date_of_birth,
                       R_date = mdy(date_of_registration),
                       Reg_bf16 = ifelse(R_date <= mdy(11082016),1,0),
                       Reg_in12 = ifelse(voting_method_pr2012!="",1,0),
                       population = ifelse(Reg_bf16==1 & Reg_in12==1,1,0),
                       voted2016 = ifelse(voting_method_pr2016!="" &      # DOUBLE CHECK THIS 8/6/20
                                          voting_method_pr2016!="N",1,0),
                       voted2012 = ifelse(voting_method_pr2012!="" &      # DOUBLE CHECK THIS 8/6/20
                                          voting_method_pr2012!="N",1,0), 
                       voted2010 = ifelse(voting_method_pr2010!="" &      # DOUBLE CHECK THIS 8/6/20
                                          voting_method_pr2010!="N",1,0),                        
                       State = "New Mexico") %>% 
       left_join(estrace, by="voterID") %>% 
       filter(population==1) %>% # ONLY KEEP THOSE WHO REGISTERED BTW 2012-2016
       mutate(estrace = est.race, VoterID = voterID) %>%
       dplyr::select(VoterID, voted2010, voted2012, voted2016, female, democrat, estrace, State, birthyear)


nm12 <- dat2 %>% mutate(Vote = voted2012, age = 2012 - birthyear, Year=2012) %>%
               dplyr:: select(VoterID, female, democrat, age, estrace, State, Vote, Year, voted2010)
  
nm16 <- dat2 %>% mutate(Vote = voted2016, age = 2016 - birthyear, Year=2016) %>%
               dplyr:: select(VoterID, female, democrat, age, estrace, State, Vote, Year, voted2010)
  
nm12_16 <- rbind(nm12,nm16)

write_csv(nm12_16, "Stack_NM_2012_2016.csv")



# CHECK
#> table(dat$voting_method_pr2016)
#
#            A      E      N      P      Y 
#342355  20802 117969 627512 187528      6 
#--> ""=No Data, A=Absentee Voting, E=Early Voting, N=Not Turnout?
#    P=Polling Place, Y=?


################################################################################################
# END OF THIS R SOURCE CODE
################################################################################################
  
