################################################################################################
# DID_BVMEffect.R
# Created by Yuki Atsusaka
# Since 8/27/2019
# Last updated 8/30/2019
# Aim: to use the difference-in-differences estimator for the ATE of the VBM policy in Colorado
################################################################################################

#########################################################################################################
# (1) DATA ORGANIZATION
# WE'LL CREATE PANEL DATA (NOT REPEATED CROSS-SECTIONAL)

# 10/23/2020 THIS DATA IS FROM ANDREW
#nc2010 <- read_dta("2010 NC Reg voters.dta") 
#nc2010id <- nc2010 %>% mutate(VoterID=ncid) %>% dplyr::select(VoterID)
#write_csv(nc2010id, "2010NCRegVotersID.csv")

#+++++++++++++++++++++++++++++++++++++++++++++#
# NORTH CAROLINA (CONTROL STATE)
#+++++++++++++++++++++++++++++++++++++++++++++#
# READING RAW DATA
rm(list=ls())
library(tidyverse)
setwd("C:/Users/YUKI/Box/FromLaptop/Project/03_ColoradoVBM_BOB/VBM_analysis")

nc2012 <- read_csv("2012 North Carolina CSV.csv") 
nc2016 <- read_csv("2016 North Carolina CSV.csv")
nc2010 <- read_csv("2010NCRegVotersID.csv") %>% pull()

# KEEP ONLY NECESSARY VARIABLES
nc2012_sl <- nc2012 %>%
             select(ncid, female, age, democrat, estrace, voted2012, voted2010) %>%
             rename(Vote = voted2012, VoterID = ncid) %>%
             filter(VoterID %in% nc2010) %>%
             mutate(Year = 2012)

nc2016_sl <- nc2016 %>%
             select(ncid, female, age, democrat, estrace, voted2016, voted2012, voted2010) %>%
             rename(Vote = voted2016, VoterID = ncid) %>%
             filter(VoterID %in% nc2010) %>%
             mutate(Year = 2016)

rm(nc2012,nc2016,nc2010); gc(); gc()

# STACK TWO YERAS
nc12_16 <- union_all(nc2012_sl, nc2016_sl) %>%  # 8279011 + 7539082 (= 15818093)
           mutate(State = "North Carolina") %>%
           filter(is.na(Vote)==F) %>%
           filter(is.na(female)==F & is.na(democrat)==F & is.na(age)==F & is.na(estrace)==F) %>%
           add_count(VoterID) %>%               # Defining POI
           filter(n==2)                      # ONLY KEEP VOTERS WHO ARE BOTH IN 2012 & 2016 (N=15078164) 

write_csv(nc12_16, "Stack_NC_2012_2016.csv")


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

 # co2010 <- read_dta("2010 Reg Co Voters and Vote History.dta")
 # voted2010 <- co2010 %>% mutate(VoterID=voted_id6) %>%
 #              dplyr::select(VoterID, voted2010)
 # write_csv(voted2010, "Colorado2010_turnout.csv")


rm(list=ls()); gc(); gc()
library(haven)
library(tidyverse)
setwd("C:/Users/YUKI/Box/FromLaptop/Project/03_ColoradoVBM_BOB/VBM_analysis")

# Read Data
co2012 <- read_csv("co2012.csv")         # VOTER FILE 2012
co2016 <- read_csv("co2016.csv")         # VOTER FILE 2016
sup_co2012 <- read_csv("co2012_sup.csv") # ESTIMATED RACE 2012
sup_co2016 <- read_csv("co2016_sup.csv") # ESTIMATED RACE 2016

# Merge with estimated race and ethnicity
co2012_sl <- co2012 %>%             # 3738665 obs
             left_join(sup_co2012, by="voter_id") %>%
             rename(Vote = voted_2012, VoterID = voter_id,
                    gender = gender1, age = age1)

co2016_sl <- co2016 %>%             # 3981191 obs 
             left_join(sup_co2016, by="voter_id") %>%
             mutate(Vote = ifelse(voterd_2016=="Voted",1,0)) %>%
             rename(VoterID = voter_id)

# STACK TWO YERAS
co12_16 <- co2012_sl %>%
           full_join(co2016_sl, by="VoterID") %>%
           mutate(State = "Colorado") %>%         # 3214220 obs
           gather(Vote.x, Vote.y, key="Merge", value="Vote") %>%
           arrange(VoterID) %>%
           mutate(Year = ifelse(Merge=="Vote.x", 2012, 2016),
                  age = ifelse(Year==2016, age+4, age)) %>%
           rename(estrace = estrace.x, female = gender) %>%
           dplyr::select(-c(voterd_2016, Merge, estrace.y, state1, white)) %>%
           filter(is.na(Vote)==F) %>% # Drop missing obs with Vote
           filter(is.na(female)==F & is.na(democrat)==F & is.na(age)==F & is.na(estrace)==F) %>%
           add_count(VoterID) %>%     # DEFINING POI
           filter(n==2)               # ONLY KEEP VOTERS WHO ARE BOTH IN 2012 & 2016

co2010 <- read_csv("Colorado2010_turnout.csv") # See line 97-100


co12_16 <- co12_16 %>% left_join(co2010, by="VoterID") # 4494348
mean(!is.na(co12_16$voted2010)) # 0.9074259 (9.3% has missing values)
mean(co12_16$voted2010, na.rm=T) # 0.6853199 more reasonable

dat.imp4 <- co12_16 %>% filter(!is.na(voted2010)) # Limiting the population
write_csv(dat.imp4, "Stack_Colorado_2012_2016.csv")


#########################################################################################################
# 2016 COLORADO + NORTH CAROLINA
#########################################################################################################
rm(list=ls());gc(); gc()
library(tidyverse)
stack_nc <- read_csv("Stack_NC_2012_2016.csv")       # 12089156
stack_nc <- stack_nc %>% select(-voted2012)

# LIMITED POPULATION
stack_co <- read_csv("Stack_Colorado_2012_2016.csv") # 4494532
stack_co <- stack_co %>% mutate(VoterID = as.character(VoterID))
stack_co_nc <- union_all(stack_co, stack_nc) %>%
               mutate(Time = ifelse(Year==2016, 1, 0),
                      Place = ifelse(State=="Colorado", 1,0),
                      Intervent = Time*Place) # 18367514
write_csv(stack_co_nc, "Stack_Colorado_NC_2012_2016.csv")

#########################################################################################################
# 2016 COLORADO + NEW MEXICO 
#########################################################################################################
rm(list=ls());gc();gc()
library(tidyverse)
stack_nm <- read_csv("Stack_NM_2012_2016.csv")       # 1616984

# LIMITED POPULATION
stack_co <- read_csv("Stack_Colorado_2012_2016.csv") # 4494532
stack_co_nm <- union_all(stack_co, stack_nm) %>%
               mutate(Time = ifelse(Year==2016, 1, 0),
                      Place = ifelse(State=="Colorado", 1,0),
                      Intervent = Time*Place) # 6111336
write_csv(stack_co_nm, "Stack_Colorado_NM_2012_2016.csv")
#########################################################################################################

#########################################################################################################
# 2014 COLORADO TURNOUT
#########################################################################################################

rm(list=ls())
library(tidyverse)

setwd("C:/Users/YUKI/Box/FromLaptop/Project/03_ColoradoVBM_BOB/VBM_analysis")

#library(haven)
#dat <- read_dta("voterfile_fixed_Nov15.dta") # Colorado 2014
#write_csv(dat, "voterfile_fixed_Nov15.csv")

dat <- read_csv("2014hist.csv") # ALL VOTER WHO VOTED (10/8/2020)
dat2 <- dat %>% filter(ELECTION_DATE=="11/04/2014") %>%
        mutate(VoterID=VOTER_ID, voted2014=1) %>%
        distinct(VoterID, voted2014) # 2072450

#dat <- read_csv("voterfile_fixed_Nov15.csv") # Same Voter appears 3 times
# temp <- dat %>% dplyr::select(vid, year, vote, elec2012, elec2014) %>%
#         arrange(vid, year)
#dat2 <- dat %>% filter(!is.na(elec2014)&year==2014) %>%
#        mutate(VoterID = vid, 
#               voted2014 = vote) %>% 
#        dplyr::select(VoterID, voted2014) 

#write_csv(dat2, "Colo2014.csv") # IF ONE IS NOT IN THIS FILE, SHE DID NOT VOTE

#########################################################################################################
# 2014 NORTH CAROLINA TURNOUT
#########################################################################################################

rm(list=ls())
library(tidyverse)

setwd("C:/Users/YUKI/Box/FromLaptop/Project/03_ColoradoVBM_BOB/VBM_analysis")

#library(haven)
# dat <- read_dta("2014 NC Voters.dta") # North Carolina 2014
# write_csv(dat, "2014 NC Voters.csv")
dat <- read_csv("2014 NC Voters.csv") # Only who voted are included here
dat2 <- dat %>% mutate(VoterID = ncid, voted2014 = 1) %>%
        dplyr::select(VoterID, voted2014)

write_csv(dat2, "NorthCarolina2014.csv")

#########################################################################################################
# 2014 COLORADO + NORTH CAROLINA 
#########################################################################################################
rm(list=ls()); gc(); gc()
library(tidyverse)
co2014 <- read_csv("Colo2014.csv")              # 2072450

stack_nc <- read_csv("Stack_NC_2012_2016.csv") # 12089156

nc2014 <- read_csv("NorthCarolina2014.csv")    # 2938967
nc2014 <- nc2014 %>% arrange(VoterID) %>% distinct(VoterID, voted2014)# Drop duplicates (2938955)
stack_nc <- stack_nc %>% left_join(nc2014, by="VoterID") # 4494352
stack_nc <- stack_nc %>% mutate(voted2014 = ifelse(!is.na(voted2014), voted2014, 0)) # CODE NOT-VOTED

stack_nc <- stack_nc %>% mutate(Vote = ifelse(Year==2012, Vote, voted2014),  # Replace 2016 with 2014 data
                                Year = ifelse(Year==2012, Year, 2014)) %>%   # Replace 2016 with 2014 data
            dplyr::select(-voted2014)
stack_nc <- stack_nc %>% select(-voted2012)



# LIMITED POPULATION
stack_co <- read_csv("Stack_Colorado_2012_2016.csv") # 4494532
stack_co <- stack_co %>% left_join(co2014, by="VoterID") # 4494352 
stack_co <- stack_co %>% mutate(voted2014 = ifelse(!is.na(voted2014), voted2014, 0)) # CODE NOT-VOTED FOT THOSE WHO WERE NOT IN "co2014.csv"

stack_co <- stack_co %>% mutate(Vote = ifelse(Year==2012, Vote, voted2014),  # Replace 2016 with 2014 data
                                Year = ifelse(Year==2012, Year, 2014)) %>%   # Replace 2016 with 2014 data
            dplyr::select(-voted2014)
stack_co <- stack_co %>% mutate(VoterID = as.character(VoterID))

stack_co_nc <- union_all(stack_co, stack_nc) %>%
               mutate(Time = ifelse(Year==2014, 1, 0),
                      Place = ifelse(State=="Colorado", 1,0),
                      Intervent = Time*Place) # 16583508
write_csv(stack_co_nc, "Stack_Colorado_NC_2012_2014.csv")


#########################################################################################################
# 2014 COLORADO + NEW MEXICO 
#########################################################################################################
rm(list=ls());gc(); gc()
library(tidyverse)
stack_nm <- read_csv("Stack_NM_2012_2014.csv") # 2006024
stack_nm <- stack_nm %>% mutate(VoterID = as.character(VoterID))
co2014 <- read_csv("Colo2014.csv")             # 2293221


# LIMITED DISTRIBUTION
stack_co <- read_csv("Stack_Colorado_2012_2016.csv") # 4494532
stack_co <- stack_co %>% left_join(co2014, by="VoterID") # 4494352 
stack_co <- stack_co %>% mutate(voted2014 = ifelse(!is.na(voted2014), voted2014, 0)) # CODE NOT-VOTED FOT THOSE WHO WERE NOT IN "co2014.csv"

stack_co <- stack_co %>% mutate(Vote = ifelse(Year==2012, Vote, voted2014),  # Replace 2016 with 2014 data
                                Year = ifelse(Year==2012, Year, 2014)) %>%   # Replace 2016 with 2014 data
            dplyr::select(-voted2014)
stack_co <- stack_co %>% mutate(VoterID = as.character(VoterID))

stack_co_nm <- union_all(stack_co, stack_nm) %>%
               mutate(Time = ifelse(Year==2014, 1, 0),
                      Place = ifelse(State=="Colorado", 1,0),
                      Intervent = Time*Place) # 16583508

write_csv(stack_co_nm, "Stack_Colorado_NM_2012_2014.csv")


#########################################################################################################
# END OF THIS R SOURCE FILE
#########################################################################################################