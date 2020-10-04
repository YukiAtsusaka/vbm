################################################################################################
# DID_BVMEffect.R
# Created by Yuki Atsusaka
# Since 8/27/2019
# Last updated 8/30/2019
# Aim: to use the difference-in-differences estimator for the ATE of the VBM policy in Colorado
################################################################################################

#########################################################################################################
# (1) DATA ORGANIZATION
# CREATE A TIDY DATA THAT IS USEFUL IN DID ESTIMATION
# WE'LL CREATE PANEL DATA (NOT REPEATED CROSS-SECTIONAL)

#+++++++++++++++++++++++++++++++++++++++++++++#
# NORTH CAROLINA (CONTROL STATE)
#+++++++++++++++++++++++++++++++++++++++++++++#
# READING RAW DATA
rm(list=ls())
library(tidyverse)
library(lubridate)
nc2012 <- read_csv("2012 North Carolina CSV.csv") 
nc2016 <- read_csv("2016 North Carolina CSV.csv")


#TEMPOLARILY 8/11/2020
R_length_NC = nc2012 %>% dplyr::select(registr_dt) %>%
              mutate(R_length = mdy(11082016) - mdy(registr_dt),
                     R_length = as.numeric(R_length)) %>%
              filter(R_length < 36500) %>%
              dplyr::select(R_length) %>% pull()

par(mfrow=c(1,1))
hist(R_length_NC/365, main="Years of Registration (North Carolina)", breaks=80)
abline(v=1, col="firebrick4", lwd=2)
abline(v=3, col="navy", lwd=2)
abline(v=5, col="black", lwd=2)
abline(v=10, col="black", lwd=2)
abline(v=20, col="black", lwd=2)



# KEEP ONLY NECESSARY VARIABLES
nc2012_sl <- nc2012 %>%
             select(ncid, female, age, democrat, estrace, voted2012, voted2010) %>%
             rename(Vote = voted2012, VoterID = ncid) %>%
             mutate(Year = 2012)

nc2016_sl <- nc2016 %>%
             select(ncid, female, age, democrat, estrace, voted2016, voted2012, voted2010) %>%
             rename(Vote = voted2016, VoterID = ncid) %>%
             mutate(Year = 2016)

# STACK TWO DATASETS INTO ONE
# MAIN POPULATION OF INTEREST
nc12_16 <- union_all(nc2012_sl, nc2016_sl) %>%  # 8279011 + 7539082 (= 15818093)
           mutate(State = "North Carolina") %>%
           filter(is.na(Vote)==F) %>%
           filter(is.na(female)==F & is.na(democrat)==F & is.na(age)==F & is.na(estrace)==F) %>%
           add_count(VoterID) %>%
           filter(n==2)                         # 15078164 obs left  
write_csv(nc12_16, "Stack_NC_2012_2016.csv")


# # SIDE- POPULATION OF INTEREST
# nc12_16 <- union_all(nc2012_sl, nc2016_sl) %>%  # 8279011 + 7539082 (= 15818093)
#   mutate(State = "North Carolina") %>%
#   filter(is.na(Vote)==F) %>%
#   filter(is.na(female)==F & is.na(democrat)==F & is.na(age)==F & is.na(estrace)==F)                      # 15078164 obs left  
# write_csv(nc12_16, "Stack_NC_2012_2016_expanded.csv")


#+++++++++++++++++++++++++++++++++++++++++++++#
# COLORADO (TREATMENT STATE)
#+++++++++++++++++++++++++++++++++++++++++++++#
# rm(list=ls()); gc(); gc()
# library(haven)
# library(tidyverse)

#co2012 <- read_dta("Colo 2012 voted.dta") # 3738665
#co2012 <- co2012 %>% distinct(voter_id, .keep_all=T) # 3642765   # Drop duplicates [A LOT, needs check]
#write_csv(co2012, "co2012.csv")

#co2016 <- read_dta("Colo 2016 voted.dta") # 3981191
#co2016 <- co2016 %>% distinct(voter_id, .keep_all=T) # 3834089   # Drop duplicates [A LOT, needs check]
#write_csv(co2016, "co2016.csv"); rm(co2016)

#sup_co2012 <- read_dta("Colo reg voters 2012 estimated race.dta") # 3738665     # Estimated race
#sup_co2012 <- sup_co2012 %>% rename(voter_id = voterid) %>%
#              distinct(voter_id, .keep_all=T) # 2706451          # Drop duplicates [A LOT, needs check]
#write_csv(sup_co2012, "co2012_sup.csv")

#sup_co2016 <- read_dta("colo 2016 with method of voting 09_17_18_2.dta") # 398XXXXX # Estimated race
#sup_co2016 <- sup_co2016 %>% select(voter_id, estrace) %>% 
#              distinct(voter_id, .keep_all=T)     # 3778507      # Drop duplicates [A LOT, needs check]
#write_csv(sup_co2016, "co2016_sup.csv")

# co2010 <- read_csv("2010 Reg Co Voters and Vote History.dta")
# voted2010 <- co2010 %>% mutate(VoterID=voted_id6) %>%
#              dplyr::select(VoterID, voted2010)
# write_csv(voted2010, "Colorado2010_turnout.csv")


rm(list=ls()); gc(); gc()
library(haven)
library(tidyverse)
setwd("C:/Users/YUKI/Box/FromLaptop/Project/03_ColoradoVBM_BOB/VBM_analysis")

# Read Data
co2012 <- read_csv("co2012.csv")
co2016 <- read_csv("co2016.csv")
sup_co2012 <- read_csv("co2012_sup.csv")
sup_co2016 <- read_csv("co2016_sup.csv")

# Merge with estimated race and ethnicity
co2012_sl <- co2012 %>%             # 3738665 obs
             left_join(sup_co2012, by="voter_id") %>%
             rename(Vote = voted_2012, VoterID = voter_id,
                    gender = gender1, age = age1)

co2016_sl <- co2016 %>%             # 3981191 obs 
             left_join(sup_co2016, by="voter_id") %>%
             mutate(Vote = ifelse(voterd_2016=="Voted",1,0)) %>%
             rename(VoterID = voter_id)


co12_16 <- co2012_sl %>%
           full_join(co2016_sl, by="VoterID") %>%
           mutate(State = "Colorado") %>%         # 3214220 obs
           gather(Vote.x, Vote.y, key="Merge", value="Vote") %>%
           arrange(VoterID) %>%
           mutate(Year = ifelse(Merge=="Vote.x", 2012, 2016),
                  age = ifelse(Year==2016, age+4, age)) %>%
           rename(estrace = estrace.x, female = gender) %>% # CHECK IF GENDER==FEMALE IN RAW DATA
           dplyr::select(-c(voterd_2016, Merge, estrace.y, state1, white)) %>%
           filter(is.na(Vote)==F) %>% # Drop missing obs with Vote
           filter(is.na(female)==F & is.na(democrat)==F & is.na(age)==F & is.na(estrace)==F) %>%
           add_count(VoterID) %>%
           filter(n==2)

#co2010 <- read_csv("Colorado_voted2010.csv") # Original data
# 10/3/2020 Use Bob'S updated data

co2010 <- read_csv("Colorado2010_turnout.csv")


co12_16 <- co12_16 %>% left_join(co2010, by="VoterID") # 4494348
mean(!is.na(co12_16$voted2010)) # 0.9074259
mean(co12_16$voted2010, na.rm=T) # 0.6853199 more reasonable


#
m <- glm(voted2010 ~ female+democrat+age+estrace, family=binomial,co12_16)

pred.val <- predict(m, co12_16[,c(2,3,4,5)], type="response")
pred_voted2010 <- ifelse(pred.val >=0.5, 1,0)

dat.imp  <- co12_16 %>% mutate(voted2010 = ifelse(!is.na(voted2010), voted2010, pred_voted2010))
dat.imp2 <- co12_16 %>% mutate(voted2010 = ifelse(!is.na(voted2010), voted2010, 0)) # Lowest Value
dat.imp3 <- co12_16 %>% mutate(voted2010 = ifelse(!is.na(voted2010), voted2010, 1)) # Highest Value

mean(dat.imp$voted2010==dat.imp2$voted2010) # 0.9468188
mean(dat.imp$voted2010==dat.imp3$voted2010) # 0.9606072
mean(dat.imp$voted2010[dat.imp$Year==2016]) #[1] 0.6802455
mean(dat.imp2$voted2010[dat.imp2$Year==2016]) #[1] 0.6218771
mean(dat.imp3$voted2010[dat.imp3$Year==2016]) #[1] 0.7144511

write_csv(dat.imp, "Stack_Colorado_2012_2016_imputed.csv")
write_csv(dat.imp2, "Stack_Colorado_2012_2016_imputed_Low.csv")
write_csv(dat.imp3, "Stack_Colorado_2012_2016_imputed_Up.csv")


# # FOR THE EXPANDED POPULATION
# co12_16 <- co2012_sl %>%
#   full_join(co2016_sl, by="VoterID") %>%
#   mutate(State = "Colorado") %>%         # 3214220 obs
#   gather(Vote.x, Vote.y, key="Merge", value="Vote") %>%
#   arrange(VoterID) %>%
#   mutate(Year = ifelse(Merge=="Vote.x", 2012, 2016),
#          age = ifelse(Year==2016, age+4, age)) %>%
#   rename(estrace = estrace.x, female = gender) %>% # CHECK IF GENDER==FEMALE IN RAW DATA
#   dplyr::select(-c(voterd_2016, Merge)) %>%
#   filter(is.na(Vote)==F) %>% # Drop missing obs with Vote
#   filter(is.na(female)==F & is.na(democrat)==F & is.na(age)==F & is.na(estrace)==F)
# #  add_count(VoterID) 
# #  filter(n==2)  # NOT DROP VOTERS WHO ARE REGISTERED ONLY IN EITHER ELECTION
# write_csv(co12_16, "Stack_Colorado_2012_2016_expanded.csv")


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# STACKING TREATMENT AND CONTROL STATES (I)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# MAIN POPULATION OF INTEREST
rm(list=ls());gc(); gc()
library(tidyverse)
library(magrittr)
stack_nc <- read_csv("Stack_NC_2012_2016.csv")       # 12089156
stack_nc <- stack_nc %>% select(-voted2012)

# LOGISTIC IMPUTATION
stack_co <- read_csv("Stack_Colorado_2012_2016_imputed.csv") # 4494532
stack_co <- stack_co %>% mutate(VoterID = as.character(VoterID))
stack_co_nc <- union_all(stack_co, stack_nc) %>%
               mutate(Time = ifelse(Year==2016, 1, 0),
                      Place = ifelse(State=="Colorado", 1,0),
                      Intervent = Time*Place) # 18367514
write_csv(stack_co_nc, "Stack_Colorado_NC_2012_2016_imputed.csv")

# LOWEST VALUE IMPUTATION
stack_co <- read_csv("Stack_Colorado_2012_2016_imputed_Low.csv") # 4494532
stack_co <- stack_co %>% mutate(VoterID = as.character(VoterID))
stack_co_nc <- union_all(stack_co, stack_nc) %>%
               mutate(Time = ifelse(Year==2016, 1, 0),
                      Place = ifelse(State=="Colorado", 1,0),
                      Intervent = Time*Place) # 18367514
write_csv(stack_co_nc, "Stack_Colorado_NC_2012_2016_imputed_Low.csv")


# HIGHEST VALUE IMPUTATION
stack_co <- read_csv("Stack_Colorado_2012_2016_imputed_Up.csv") # 4494532
stack_co <- stack_co %>% mutate(VoterID = as.character(VoterID))
stack_co_nc <- union_all(stack_co, stack_nc) %>%
               mutate(Time = ifelse(Year==2016, 1, 0),
                      Place = ifelse(State=="Colorado", 1,0),
                      Intervent = Time*Place) # 18367514
write_csv(stack_co_nc, "Stack_Colorado_NC_2012_2016_imputed_Up.csv")


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# STACKING TREATMENT AND CONTROL STATES (II) 8/6/2020, 10/4/2020 updated
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# MAIN POPULATION OF INTEREST
rm(list=ls());gc();gc()
library(tidyverse)
stack_nm <- read_csv("Stack_NM_2012_2016.csv")       # 1616984


# LOGISTIC IMPUTATION
stack_co <- read_csv("Stack_Colorado_2012_2016_imputed.csv") # 4494532
stack_co_nm <- union_all(stack_co, stack_nm) %>%
               mutate(Time = ifelse(Year==2016, 1, 0),
                      Place = ifelse(State=="Colorado", 1,0),
                      Intervent = Time*Place) # 6111336
write_csv(stack_co_nm, "Stack_Colorado_NM_2012_2016_imputed.csv")


# LOWEST VALUE IMPUTATION
stack_co <- read_csv("Stack_Colorado_2012_2016_imputed_Low.csv") # 4494532
stack_co_nm <- union_all(stack_co, stack_nm) %>%
               mutate(Time = ifelse(Year==2016, 1, 0),
                      Place = ifelse(State=="Colorado", 1,0),
                      Intervent = Time*Place) # 6111336
write_csv(stack_co_nm, "Stack_Colorado_NM_2012_2016_imputed_Low.csv")


# HIGHEST VALUE IMPUTATION
stack_co <- read_csv("Stack_Colorado_2012_2016_imputed_Up.csv") # 4494532
stack_co_nm <- union_all(stack_co, stack_nm) %>%
               mutate(Time = ifelse(Year==2016, 1, 0),
                      Place = ifelse(State=="Colorado", 1,0),
                      Intervent = Time*Place) # 6111336
write_csv(stack_co_nm, "Stack_Colorado_NM_2012_2016_imputed_Up.csv")
#########################################################################################################

#########################################################################################################
# 8/14/2020 EXTRACT 2014 COLORADO TURNOUT
#########################################################################################################

rm(list=ls())
library(tidyverse)
library(haven)

setwd("C:/Users/YUKI/Box/FromLaptop/Project/03_ColoradoVBM_BOB/VBM_analysis")

#dat <- read_dta("voterfile_fixed_Nov15.dta") # Colorado 2014
#write_csv(dat, "voterfile_fixed_Nov15.csv")
dat <- read_csv("voterfile_fixed_Nov15.csv") # Same Voter appears 3 times
# temp <- dat %>% dplyr::select(vid, year, vote, elec2012, elec2014) %>%
#         arrange(vid, year)
dat2 <- dat %>% filter(!is.na(elec2014)  & year==2014) %>%
        mutate(VoterID = vid, 
               voted2014 = vote) %>% 
        dplyr::select(VoterID, voted2014) 

write_csv(dat2, "Colo2014.csv")

#########################################################################################################
# 8/18/2020 EXTRACT 2014 NORTH CAROLINA TURNOUT
#########################################################################################################

rm(list=ls())
library(tidyverse)
library(haven)

setwd("C:/Users/YUKI/Box/FromLaptop/Project/03_ColoradoVBM_BOB/VBM_analysis")

# dat <- read_dta("2014 NC Voters.dta") # North Carolina 2014
# write_csv(dat, "2014 NC Voters.csv")
dat <- read_csv("2014 NC Voters.csv") # Only who voted are included here
dat2 <- dat %>% mutate(VoterID = ncid, voted2014 = 1) %>%
        dplyr::select(VoterID, voted2014)

write_csv(dat2, "NorthCarolina2014.csv")



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# STACKING TREATMENT AND CONTROL STATES (III) 8/14/2020, 8/18/2020
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# MAIN POPULATION OF INTEREST
rm(list=ls()); gc(); gc()
library(tidyverse)
co2014 <- read_csv("Colo2014.csv")                   # 2293221
co2014 <- co2014 %>% arrange(VoterID) %>% distinct(VoterID, voted2014)# Drop duplicates

stack_nc <- read_csv("Stack_NC_2012_2016.csv") # 12089156

nc2014 <- read_csv("NorthCarolina2014.csv")    # 2938955 
nc2014 <- nc2014 %>% arrange(VoterID) %>% distinct(VoterID, voted2014)# Drop duplicates
stack_nc <- stack_nc %>% left_join(nc2014, by="VoterID") # 4494352
stack_nc <- stack_nc %>% mutate(voted2014 = ifelse(!is.na(voted2014), voted2014, 0))

stack_nc <- stack_nc %>% mutate(Vote = ifelse(Year==2012, Vote, voted2014),  # Replace 2016 with 2014 data
                                Year = ifelse(Year==2012, Year, 2014)) %>%   # Replace 2016 with 2014 data
            dplyr::select(-voted2014)


stack_nc <- stack_nc %>% select(-voted2012)


# LOGISTIC IMPUTATION
stack_co <- read_csv("Stack_Colorado_2012_2016_imputed.csv") # 4494532
stack_co <- stack_co %>% left_join(co2014, by="VoterID") # 4494352 
stack_co <- stack_co %>% mutate(Vote = ifelse(Year==2012, Vote, voted2014),  # Replace 2016 with 2014 data
                                Year = ifelse(Year==2012, Year, 2014)) %>%   # Replace 2016 with 2014 data
            dplyr::select(-voted2014)
stack_co <- stack_co %>% mutate(VoterID = as.character(VoterID))

stack_co_nc <- union_all(stack_co, stack_nc) %>%
               mutate(Time = ifelse(Year==2014, 1, 0),
                      Place = ifelse(State=="Colorado", 1,0),
                      Intervent = Time*Place) # 16583508
write_csv(stack_co_nc, "Stack_Colorado_NC_2012_2014_imputed.csv")



# LOWEST VALUE IMPUTATION
stack_co <- read_csv("Stack_Colorado_2012_2016_imputed_Low.csv") # 4494532
stack_co <- stack_co %>% left_join(co2014, by="VoterID") # 4494352 
stack_co <- stack_co %>% mutate(Vote = ifelse(Year==2012, Vote, voted2014),  # Replace 2016 with 2014 data
                                Year = ifelse(Year==2012, Year, 2014)) %>%   # Replace 2016 with 2014 data
            dplyr::select(-voted2014)
stack_co <- stack_co %>% mutate(VoterID = as.character(VoterID))

stack_co_nc <- union_all(stack_co, stack_nc) %>%
               mutate(Time = ifelse(Year==2014, 1, 0),
                      Place = ifelse(State=="Colorado", 1,0),
                      Intervent = Time*Place) # 16583508
write_csv(stack_co_nc, "Stack_Colorado_NC_2012_2014_imputed_Low.csv")


# HIGHEST VALUE IMPUTATION
stack_co <- read_csv("Stack_Colorado_2012_2016_imputed_Up.csv") # 4494532
stack_co <- stack_co %>% left_join(co2014, by="VoterID") # 4494352 
stack_co <- stack_co %>% mutate(Vote = ifelse(Year==2012, Vote, voted2014),  # Replace 2016 with 2014 data
                                Year = ifelse(Year==2012, Year, 2014)) %>%   # Replace 2016 with 2014 data
            dplyr::select(-voted2014)
stack_co <- stack_co %>% mutate(VoterID = as.character(VoterID))

stack_co_nc <- union_all(stack_co, stack_nc) %>%
               mutate(Time = ifelse(Year==2014, 1, 0),
                      Place = ifelse(State=="Colorado", 1,0),
                      Intervent = Time*Place) # 16583508
write_csv(stack_co_nc, "Stack_Colorado_NC_2012_2014_imputed_Up.csv")



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# STACKING TREATMENT AND CONTROL STATES (III) 8/14/2020, 8/18/2020
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# MAIN POPULATION OF INTEREST

rm(list=ls());gc(); gc()
#library(tidyverse)
stack_nm <- read_csv("Stack_NM_2012_2014.csv") # 2006024
stack_nm <- stack_nm %>% mutate(VoterID = as.character(VoterID))
co2014 <- read_csv("Colo2014.csv")                   # 2293221
co2014 <- co2014 %>% arrange(VoterID) %>% distinct(VoterID, voted2014)# Drop duplicates


# LOGISTIC IMPUTATION
stack_co <- read_csv("Stack_Colorado_2012_2016_imputed.csv") # 4494532
stack_co <- stack_co %>% left_join(co2014, by="VoterID") # 4494352 
stack_co <- stack_co %>% mutate(Vote = ifelse(Year==2012, Vote, voted2014),  # Replace 2016 with 2014 data
                                Year = ifelse(Year==2012, Year, 2014)) %>%   # Replace 2016 with 2014 data
            dplyr::select(-voted2014)
stack_co <- stack_co %>% mutate(VoterID = as.character(VoterID))

stack_co_nm <- union_all(stack_co, stack_nm) %>%
               mutate(Time = ifelse(Year==2014, 1, 0),
                      Place = ifelse(State=="Colorado", 1,0),
                      Intervent = Time*Place) # 16583508

write_csv(stack_co_nm, "Stack_Colorado_NM_2012_2014_imputed.csv")



# LOWEST VALUE IMPUTATION
stack_co <- read_csv("Stack_Colorado_2012_2016_imputed_Low.csv") # 4494532
stack_co <- stack_co %>% left_join(co2014, by="VoterID") # 4494352 
stack_co <- stack_co %>% mutate(Vote = ifelse(Year==2012, Vote, voted2014),  # Replace 2016 with 2014 data
                                Year = ifelse(Year==2012, Year, 2014)) %>%   # Replace 2016 with 2014 data
            dplyr::select(-voted2014)
stack_co <- stack_co %>% mutate(VoterID = as.character(VoterID))

stack_co_nm <- union_all(stack_co, stack_nm) %>%
               mutate(Time = ifelse(Year==2014, 1, 0),
                      Place = ifelse(State=="Colorado", 1,0),
                      Intervent = Time*Place) # 16583508

write_csv(stack_co_nm, "Stack_Colorado_NM_2012_2014_imputed_Low.csv")



# LOWEST VALUE IMPUTATION
stack_co <- read_csv("Stack_Colorado_2012_2016_imputed_Up.csv") # 4494532
stack_co <- stack_co %>% left_join(co2014, by="VoterID") # 4494352 
stack_co <- stack_co %>% mutate(Vote = ifelse(Year==2012, Vote, voted2014),  # Replace 2016 with 2014 data
                                Year = ifelse(Year==2012, Year, 2014)) %>%   # Replace 2016 with 2014 data
            dplyr::select(-voted2014)
stack_co <- stack_co %>% mutate(VoterID = as.character(VoterID))

stack_co_nm <- union_all(stack_co, stack_nm) %>%
               mutate(Time = ifelse(Year==2014, 1, 0),
                      Place = ifelse(State=="Colorado", 1,0),
                      Intervent = Time*Place) # 16583508

write_csv(stack_co_nm, "Stack_Colorado_NM_2012_2014_imputed_Up.csv")

#########################################################################################################
# # SIDE-POPULATION OF INTEREST
# rm(list=ls())
# library(tidyverse)
# library(magrittr)
# stack_co <- read_csv("Stack_Colorado_2012_2016_expanded.csv")
# stack_nc <- read_csv("Stack_NC_2012_2016_expanded.csv")
# stack_co <- stack_co %>% mutate(VoterID = as.character(VoterID))
# stack_co <- stack_co %>% mutate(voted2012 = NA, voted2010 = NA)
# 
# stack_co_nc <- union_all(stack_co, stack_nc) %>%
#   mutate(Time = ifelse(Year==2016, 1, 0),
#          Place = ifelse(State=="Colorado", 1,0),
#          Intervent = Time*Place) # 18367514
# 
# write_csv(stack_co_nc, "Stack_Colorado_NC_2012_2016_expanded.csv")
#########################################################################################################



#########################################################################################################
# END OF THIS R SOURCE FILE
#########################################################################################################