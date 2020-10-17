################################################################################################
# DID_BVMEffect_Estimation_NM.R
# Created by Yuki Atsusaka
# Since 8/27/2019
# Last updated 8/6/2020
# Aim: to use the difference-in-differences estimator for the ATE of the VBM policy in Colorado
################################################################################################

##################################################s##############################################
# (1) SIMPLE RANDOM SAMPLING
rm(list=ls())
library(tidyverse);gc();gc()

setwd("C:/Users/YUKI/Box/FromLaptop/Project/03_ColoradoVBM_BOB/VBM_analysis")

dat <- read_csv("Stack_Colorado_NM_2012_2014.csv", col_types = cols(VoterID = col_character())) # PRIMARY POPULATION OF INTEREST
dat <- dat %>% dplyr::select(-n)
dat$age[dat$Year==2014 & dat$State=="Colorado"] <- dat$age[dat$Year==2012 & dat$State=="Colorado"] + 2


# OLD FILES NEEDED IMPUTATION
#dat$Vote[is.na(dat$Vote)] <- 1 # This leads to 0.8386044 (LESS PLAUSIBLE)
#dat$Vote[is.na(dat$Vote)] <- 0  # This leads to 0.6461022 (MORE PLAUSIBLE)

datCO <- dat %>% filter(Place==1)
datNM <- dat %>% filter(Place==0)

# TURNOUT BY STATE AND YEAR
mean(datCO$voted2010) # CO 2010 (0.6853199)
mean(datNM$voted2010) # NM 2010 (0.583102) 
mean(datCO$Vote[datCO$Year==2012]) # CO 2012 (0.8280877)
mean(datCO$Vote[datCO$Year==2014]) # CO 2014 (0.7035609)
mean(datNM$Vote[datNM$Year==2012]) # NM 2012 (0.6974762)
mean(datNM$Vote[datNM$Year==2014]) # NM 2014 (0.4800759)

mean(!is.na(datCO$Vote[datCO$Year==2012])) # 1 OK
mean(!is.na(datCO$Vote[datCO$Year==2014])) # 1 OK
mean(!is.na(datNM$Vote[datNM$Year==2012])) # 1 OK
mean(!is.na(datNM$Vote[datNM$Year==2014])) # 1 OK


# SIMPLE RANDOM SAMPLING (All Voters) ==================================================#
set.seed(1029501)
sample_CO <- datCO %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
             sample(size=round(0.01*dim(datCO)[1]), replace=F)   # SAMPLE 1% = 8988
datCO.s <- datCO %>% filter(VoterID %in% sample_CO) %>% arrange(VoterID)

set.seed(1029501)
sample_NM <- datNM %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
             sample(size=round(0.1*dim(datNM)[1]), replace=F)   # SAMPLE 1% = 24178
datNM.s <- datNM %>% filter(VoterID %in% sample_NM) %>% arrange(VoterID)

dat_samp <- union_all(datCO.s, datNM.s) # Stack two states again
write_csv(dat_samp, "1_NM2014_Sample.csv")
########################################################################################

# SIMPLE RANDOM SAMPLING (Frequent Voters) ==================================================#
set.seed(1029501)
sample_CO <- datCO %>% filter(voted2010==1) %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.03*dim(datCO)[1]), replace=F)   # SAMPLE 1% = 8988
datCO.s <- datCO %>% filter(VoterID %in% sample_CO) %>% arrange(VoterID)

set.seed(1029501)
sample_NM <- datNM %>% filter(voted2010==1) %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.2*dim(datNM)[1]), replace=F)   # SAMPLE 1% = 24178
datNM.s <- datNM %>% filter(VoterID %in% sample_NM) %>% arrange(VoterID)

dat_samp <- union_all(datCO.s, datNM.s) # Stack two states again
write_csv(dat_samp, "2_NM2014_SampleFreq.csv")
################################################################################################


# SIMPLE RANDOM SAMPLING (Infrequent Voters) ==================================================#
set.seed(1029501)
sample_CO <- datCO %>% filter(voted2010==0) %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.03*dim(datCO)[1]), replace=F)   # SAMPLE 1% = 8988
datCO.s <- datCO %>% filter(VoterID %in% sample_CO) %>% arrange(VoterID)

set.seed(1029501)
sample_NM <- datNM %>% filter(voted2010==0) %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.2*dim(datNM)[1]), replace=F)   # SAMPLE 1% = 24178
datNM.s <- datNM %>% filter(VoterID %in% sample_NM) %>% arrange(VoterID)

dat_samp <- union_all(datCO.s, datNM.s) # Stack two states again
write_csv(dat_samp, "3_NM2014_SampleInfreq.csv")
################################################################################################


# SIMPLE RANDOM SAMPLING (Young less than 35 Voters) ==================================================#
set.seed(1029501)
sample_CO <- datCO %>% filter(age<=40) %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.01*dim(datCO)[1]), replace=F)   # SAMPLE 1% = 8988
datCO.s <- datCO %>% filter(VoterID %in% sample_CO) %>% arrange(VoterID)

set.seed(1029501)
sample_NM <- datNM %>% filter(age<=40) %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.1*dim(datNM)[1]), replace=F)   # SAMPLE 1% = 24178
datNM.s <- datNM %>% filter(VoterID %in% sample_NM) %>% arrange(VoterID)

dat_samp <- union_all(datCO.s, datNM.s) # Stack two states again
write_csv(dat_samp, "4_NM2014_SampleYoung.csv")
################################################################################################



# SIMPLE RANDOM SAMPLING (Middle > 35, < 65  Voters) ==================================================#
set.seed(1029501)
sample_CO <- datCO %>% filter(age>40 & age<=65) %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.01*dim(datCO)[1]), replace=F)   # SAMPLE 1% = 8988
datCO.s <- datCO %>% filter(VoterID %in% sample_CO) %>% arrange(VoterID)

set.seed(1029501)
sample_NM <- datNM %>% filter(age>40 & age<=65) %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.1*dim(datNM)[1]), replace=F)   # SAMPLE 1% = 24178
datNM.s <- datNM %>% filter(VoterID %in% sample_NM) %>% arrange(VoterID)

dat_samp <- union_all(datCO.s, datNM.s) # Stack two states again
write_csv(dat_samp, "5_NM2014_SampleMid.csv")
################################################################################################


# SIMPLE RANDOM SAMPLING (Old over 65 Voters) ==================================================#
set.seed(1029501)
sample_CO <- datCO %>% filter(age>65) %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.01*dim(datCO)[1]), replace=F)   # SAMPLE 1% = 8988
datCO.s <- datCO %>% filter(VoterID %in% sample_CO) %>% arrange(VoterID)

set.seed(1029501)
sample_NM <- datNM %>% filter(age>65) %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.1*dim(datNM)[1]), replace=F)   # SAMPLE 1% = 24178
datNM.s <- datNM %>% filter(VoterID %in% sample_NM) %>% arrange(VoterID)

dat_samp <- union_all(datCO.s, datNM.s) # Stack two states again
write_csv(dat_samp, "6_NM2014_SampleOld.csv")
################################################################################################



# SIMPLE RANDOM SAMPLING (White Voters) ==================================================#
set.seed(1029501)
sample_CO <- datCO %>% filter(estrace=="White") %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.01*dim(datCO)[1]), replace=F)   # SAMPLE 1% = 8988
datCO.s <- datCO %>% filter(VoterID %in% sample_CO) %>% arrange(VoterID)

set.seed(1029501)
sample_NM <- datNM %>% filter(estrace=="White") %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.1*dim(datNM)[1]), replace=F)   # SAMPLE 1% = 24178
datNM.s <- datNM %>% filter(VoterID %in% sample_NM) %>% arrange(VoterID)

dat_samp <- union_all(datCO.s, datNM.s) # Stack two states again
write_csv(dat_samp, "7_NM2014_SampleWhite.csv")
########################################################################################


# SIMPLE RANDOM SAMPLING (Black Voters) ==================================================#
set.seed(1029501)
sample_CO <- datCO %>% filter(estrace=="Black") %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.01*dim(datCO)[1]), replace=F)   # SAMPLE 1% = 8988
datCO.s <- datCO %>% filter(VoterID %in% sample_CO) %>% arrange(VoterID)

set.seed(1029501)
sample_NM <- datNM %>% filter(estrace=="Black") %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.005*dim(datNM)[1]), replace=F)   # SAMPLE 1% = 24178
datNM.s <- datNM %>% filter(VoterID %in% sample_NM) %>% arrange(VoterID)

dat_samp <- union_all(datCO.s, datNM.s) # Stack two states again
write_csv(dat_samp, "8_NM2014_SampleBlack.csv")
########################################################################################


# SIMPLE RANDOM SAMPLING (Hispanic Voters) ==================================================#
set.seed(1029501)
sample_CO <- datCO %>% filter(estrace=="Hispanic") %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.03*dim(datCO)[1]), replace=F)   # SAMPLE 1% = 8988
datCO.s <- datCO %>% filter(VoterID %in% sample_CO) %>% arrange(VoterID)

set.seed(1029501)
sample_NM <- datNM %>% filter(estrace=="Hispanic") %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.1*dim(datNM)[1]), replace=F)   # SAMPLE 1% = 24178
datNM.s <- datNM %>% filter(VoterID %in% sample_NM) %>% arrange(VoterID)

dat_samp <- union_all(datCO.s, datNM.s) # Stack two states again
write_csv(dat_samp, "9_NM2014_SampleHispanic.csv")
################################################################################################


# SIMPLE RANDOM SAMPLING (Asian Voters) ==================================================#
set.seed(1029501)
sample_CO <- datCO %>% filter(estrace=="Asian") %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.02*dim(datCO)[1]), replace=F)   # SAMPLE 1% = 8988
datCO.s <- datCO %>% filter(VoterID %in% sample_CO) %>% arrange(VoterID)

set.seed(1029501)
sample_NM <- datNM %>% filter(estrace=="Asian") %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.004*dim(datNM)[1]), replace=F)   # SAMPLE 1% = 24178
datNM.s <- datNM %>% filter(VoterID %in% sample_NM) %>% arrange(VoterID)

dat_samp <- union_all(datCO.s, datNM.s) # Stack two states again
write_csv(dat_samp, "X10_NM2014_SampleAsian.csv")
################################################################################################


# SIMPLE RANDOM SAMPLING (Male Voters) ==================================================#
set.seed(1029501)
sample_CO <- datCO %>% filter(female==0) %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.01*dim(datCO)[1]), replace=F)   # SAMPLE 1% = 8988
datCO.s <- datCO %>% filter(VoterID %in% sample_CO) %>% arrange(VoterID)

set.seed(1029501)
sample_NM <- datNM %>% filter(female==0) %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.1*dim(datNM)[1]), replace=F)   # SAMPLE 1% = 24178
datNM.s <- datNM %>% filter(VoterID %in% sample_NM) %>% arrange(VoterID)

dat_samp <- union_all(datCO.s, datNM.s) # Stack two states again
write_csv(dat_samp, "X11_NM2014_SampleFemale.csv")
################################################################################################


# SIMPLE RANDOM SAMPLING (Female Voters) ==================================================#
set.seed(1029501)
sample_CO <- datCO %>% filter(female==1) %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.01*dim(datCO)[1]), replace=F)   # SAMPLE 1% = 8988
datCO.s <- datCO %>% filter(VoterID %in% sample_CO) %>% arrange(VoterID)

set.seed(1029501)
sample_NM <- datNM %>% filter(female==1) %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.1*dim(datNM)[1]), replace=F)   # SAMPLE 1% = 24178
datNM.s <- datNM %>% filter(VoterID %in% sample_NM) %>% arrange(VoterID)

dat_samp <- union_all(datCO.s, datNM.s) # Stack two states again
write_csv(dat_samp, "X12_NM2014_SampleMale.csv")
################################################################################################


# SIMPLE RANDOM SAMPLING (Democratic Voters) ==================================================#
set.seed(1029501)
sample_CO <- datCO %>% filter(democrat==1) %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.01*dim(datCO)[1]), replace=F)   # SAMPLE 1% = 8988
datCO.s <- datCO %>% filter(VoterID %in% sample_CO) %>% arrange(VoterID)

set.seed(1029501)
sample_NM <- datNM %>% filter(democrat==1) %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.1*dim(datNM)[1]), replace=F)   # SAMPLE 1% = 24178
datNM.s <- datNM %>% filter(VoterID %in% sample_NM) %>% arrange(VoterID)

dat_samp <- union_all(datCO.s, datNM.s) # Stack two states again
write_csv(dat_samp, "X13_NM2014_SampleDem.csv")
################################################################################################


# SIMPLE RANDOM SAMPLING (Republican Voters) ==================================================#
set.seed(1029501)
sample_CO <- datCO %>% filter(democrat==0) %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.01*dim(datCO)[1]), replace=F)   # SAMPLE 1% = 8988
datCO.s <- datCO %>% filter(VoterID %in% sample_CO) %>% arrange(VoterID)

set.seed(1029501)
sample_NM <- datNM %>% filter(democrat==0) %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.1*dim(datNM)[1]), replace=F)   # SAMPLE 1% = 24178
datNM.s <- datNM %>% filter(VoterID %in% sample_NM) %>% arrange(VoterID)

dat_samp <- union_all(datCO.s, datNM.s) # Stack two states again
write_csv(dat_samp, "X14_NM2014_SampleNonDem.csv")
################################################################################################
