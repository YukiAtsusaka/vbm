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
library(magrittr)
nc2012 <- read_csv("2012 North Carolina CSV.csv") 
nc2016 <- read_csv("2016 North Carolina CSV.csv")

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


# SIDE- POPULATION OF INTEREST
nc12_16 <- union_all(nc2012_sl, nc2016_sl) %>%  # 8279011 + 7539082 (= 15818093)
  mutate(State = "North Carolina") %>%
  filter(is.na(Vote)==F) %>%
  filter(is.na(female)==F & is.na(democrat)==F & is.na(age)==F & is.na(estrace)==F)                      # 15078164 obs left  
write_csv(nc12_16, "Stack_NC_2012_2016_expanded.csv")




#+++++++++++++++++++++++++++++++++++++++++++++#
# COLORADO (TREATMENT STATE)
#+++++++++++++++++++++++++++++++++++++++++++++#
rm(list=ls()); gc(); gc()
library(haven)
library(tidyverse)
library(magrittr)

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

# Warning message:
#   Column `voter_id` has different attributes on LHS and RHS of join

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

co2010 <- read_csv("Colorado_voted2010.csv")
co12_16 <- co12_16 %>% left_join(co2010, by="VoterID") # 4484352

write_csv(co12_16, "Stack_Colorado_2012_2016.csv")



# FOR THE EXPANDED POPULATION
co12_16 <- co2012_sl %>%
  full_join(co2016_sl, by="VoterID") %>%
  mutate(State = "Colorado") %>%         # 3214220 obs
  gather(Vote.x, Vote.y, key="Merge", value="Vote") %>%
  arrange(VoterID) %>%
  mutate(Year = ifelse(Merge=="Vote.x", 2012, 2016),
         age = ifelse(Year==2016, age+4, age)) %>%
  rename(estrace = estrace.x, female = gender) %>% # CHECK IF GENDER==FEMALE IN RAW DATA
  dplyr::select(-c(voterd_2016, Merge)) %>%
  filter(is.na(Vote)==F) %>% # Drop missing obs with Vote
  filter(is.na(female)==F & is.na(democrat)==F & is.na(age)==F & is.na(estrace)==F)
#  add_count(VoterID) 
#  filter(n==2)  # NOT DROP VOTERS WHO ARE REGISTERED ONLY IN EITHER ELECTION
write_csv(co12_16, "Stack_Colorado_2012_2016_expanded.csv")


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# STACKING TREATMENT AND CONTROL STATES (I)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# MAIN POPULATION OF INTEREST
rm(list=ls())
library(tidyverse)
library(magrittr)
stack_co <- read_csv("Stack_Colorado_2012_2016.csv") # 4494532
stack_nc <- read_csv("Stack_NC_2012_2016.csv")       # 12089156
stack_co <- stack_co %>% mutate(VoterID = as.character(VoterID))
stack_nc <- stack_nc %>% select(-voted2012)

stack_co_nc <- union_all(stack_co, stack_nc) %>%
               mutate(Time = ifelse(Year==2016, 1, 0),
                      Place = ifelse(State=="Colorado", 1,0),
                      Intervent = Time*Place) # 18367514

write_csv(stack_co_nc, "Stack_Colorado_NC_2012_2016.csv")


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# STACKING TREATMENT AND CONTROL STATES (II) 8/6/2020
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# MAIN POPULATION OF INTEREST
rm(list=ls())
library(tidyverse)
library(magrittr)
stack_co <- read_csv("Stack_Colorado_2012_2016.csv") # 4494532
stack_nm <- read_csv("Stack_NM_2012_2016.csv")       # 1616984

stack_co_nm <- union_all(stack_co, stack_nm) %>%
               mutate(Time = ifelse(Year==2016, 1, 0),
                      Place = ifelse(State=="Colorado", 1,0),
                      Intervent = Time*Place) # 6111336

write_csv(stack_co_nm, "Stack_Colorado_NM_2012_2016.csv")
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