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
setwd("C:/Users/YUKI/Box/FromLaptop/Project/03_ColoradoVBM_BOB/VBM_analysis")

rm(list=ls()); gc(); gc()
library(tidyverse)
library(haven)
library(wru)

#setwd("C:/Users/YUKI/Box/FromLaptop/Project/03_ColoradoVBM_BOB/VBM_analysis")
# dat <- read_dta("new mexico voter file 2017.dta")
# zip <- read_csv("NM_ZipCounty.csv")
# ccode <- read_csv("NM_CountyCode.csv")  
# ccode$county[nchar(ccode$county)==2] <- paste("0", ccode$county[nchar(ccode$county)==2], sep="")
# ccode$county[nchar(ccode$county)==1] <- paste("00", ccode$county[nchar(ccode$county)==1], sep="")
# 
# dat2 <- dat %>% mutate(surname = text_name_last,
#                        voterID = text_registrant_id,
#                        zip=text_res_zip5,
#                        state="NM") %>%
#         left_join(zip, by="zip") %>%
#         left_join(ccode, by="name") %>%
#         dplyr::select(surname, voterID, county, state)
# 
# #length(unique(dat2$county)) # Check, we have 33 counties
# #unique(sort(ccode$name)) == unique(sort(zip$name)) # Check
# #mean(!is.na(dat2$county))
# dat2 <- dat2 %>% mutate(county= ifelse(is.na(county), "049", county))
# write_csv(dat2, "NewMexico_surname.csv")


df = read_csv("NewMexico_surname.csv")
df$surname = gsub("[\U4E00-\U9FFF\U3000-\U303F]", "", df$surname) # Drop Error-based Chinese Letter

############################################################################
# PREDICT RACE BASED ON SURNAME AND COUNTY
Cen.county <- get_census_data(key="6ed7597beb9cace157dc97dbf52976dfbdf2c420",
                             states="NM",age=F,sex=F,census.geo="county")
out <- predict_race(df, surname.year=2010, 
                    census.geo = "county", census.data = Cen.county) 
############################################################################

#############################################################################
## PREDICT RACE BASED ON SURNAME-ONLY METHOD
#out <- predict_race(df, surname.year=2010, surname.only=T) 
#############################################################################

 est.c <- out[, 5:9] # ONLY EXTRACT RACE PROBABILITIES TO SPEED UP ALGORITHM
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
rm(list=ls()); gc(); gc()
library(tidyverse)
library(haven)
library(lubridate)
setwd("C:/Users/YUKI/Box/FromLaptop/Project/03_ColoradoVBM_BOB/VBM_analysis")


estrace <- read_csv("NewMexico_Race.csv")
dat <- read_dta("new mexico voter file 2017.dta")

# TEMPOLARITY (DON'T KNOW IF THIS IS THE RIGHT WAY)
#dat.s <- dat %>% filter(voting_method_gn2016!=""& # FIXED pt to gn 8/11/2020
#                        voting_method_gn2012!="")

dat2 <- dat %>% mutate(voterID = text_registrant_id,
                       female =  ifelse(cde_gender=="F",1,0),
                       democrat = ifelse(desc_party=="DEMOCRAT",1,0),
                       birthyear = date_of_birth,
                       R_date = mdy(date_of_registration),
                       R_length = mdy(11082016) - 
                                  mdy(date_of_registration), 
                       R_length = as.numeric(R_length),
                       Reg_bf16 = ifelse(R_date <= mdy(11082016),1,0),
                       Reg_in16 = ifelse(voting_method_gn2016!="",1,0),
                       Reg_in12 = ifelse(voting_method_gn2012!="",1,0),
                       population = ifelse(Reg_bf16==1 & Reg_in16 & Reg_in12==1,1,0),
                       voted2016 = ifelse(voting_method_gn2016 %in% c("E", "A", "P"),1,0),
                       voted2014 = ifelse(voting_method_gn2014 %in% c("E", "A", "P"),1,0),    # DOUBLE CHECK THIS 8/6/20
                       voted2012 = ifelse(voting_method_gn2012 %in% c("E", "A", "P"),1,0),    # DOUBLE CHECK THIS 8/6/20
                       voted2010 = ifelse(voting_method_gn2010 %in% c("E", "A", "P"),1,0),     # DOUBLE CHECK THIS 8/6/20
                       State = "New Mexico") %>% 
       left_join(estrace, by="voterID") 

# CHECK THE CODING OF THE POPULATION OF INTEREST
# check <- dat2 %>%
#          dplyr::select(population, R_date, Reg_bf16, Reg_in16, Reg_in12,
#                       voting_method_pr2016, voting_method_pr2012)

mdy(11082016) - mdy(11062012)


dat3 <- dat2 %>%
        filter(population==1) %>% # ONLY KEEP THOSE WHO REGISTERED BTW 2012-2016
        mutate(estrace = est.race, VoterID = voterID) %>%
        filter(R_length < 36500) %>% # Registered less than 100 years
        filter(R_length >= 1463) %>% # Registered before 2012 election
        dplyr::select(VoterID, voted2010, voted2012, voted2014, voted2016, female, democrat, estrace, State, birthyear, R_length)

hist(dat3$R_length/365, main="Years of Registration (New Mexico)", breaks=80)
abline(v=1, col="firebrick4", lwd=2)
abline(v=3, col="navy", lwd=2)
abline(v=5, col="black", lwd=2)
abline(v=10, col="black", lwd=2)
abline(v=20, col="black", lwd=2)


nm12 <- dat3 %>% mutate(Vote = voted2012, age = 2012 - birthyear, Year=2012) %>%
               dplyr:: select(VoterID, female, democrat, age, estrace, State, Vote, Year, voted2010)
nm14 <- dat3 %>% mutate(Vote = voted2014, age = 2014 - birthyear, Year=2014) %>%
               dplyr:: select(VoterID, female, democrat, age, estrace, State, Vote, Year, voted2010)

nm16 <- dat3 %>% mutate(Vote = voted2016, age = 2016 - birthyear, Year=2016) %>%
               dplyr:: select(VoterID, female, democrat, age, estrace, State, Vote, Year, voted2010)
  
nm12_16 <- rbind(nm12,nm16)
nm12_14 <- rbind(nm12,nm14)

write_csv(nm12_16, "Stack_NM_2012_2016.csv")
write_csv(nm12_14, "Stack_NM_2012_2014.csv")

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
  
