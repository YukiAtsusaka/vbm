################################################################################################
# DID_BVMEffect_Estimation.R
# Created by Yuki Atsusaka
# Since 8/27/2019
# Last updated 8/6/2020
# Aim: to use the difference-in-differences estimator for the ATE of the VBM policy in Colorado
################################################################################################

################################################################################################
# (1) SIMPLE RANDOM SAMPLING
rm(list=ls())
library(tidyverse)
setwd("./data")

dat <- read_csv("Stack_Colorado_NC_2012_2016.csv", col_types = cols(VoterID = col_character())) # PRIMARY POPULATION OF INTEREST
datCO <- dat %>% filter(Place==1)
datNC <- dat %>% filter(Place==0) # 10870184 

dt <- datNC %>% group_by(VoterID) %>% 
      summarise_at(vars(voted2010, female, democrat), sum) %>%
      ungroup()
id <- dt %>% filter(voted2010!=1 & female!=1 & democrat !=1 ) %>%
      dplyr::select(VoterID) %>% pull()
datNC <- datNC %>% filter(VoterID %in% id) # 10496246 


# TURNOUT BY STATE AND YEAR
mean(datCO$voted2010) # CO 2010 (0.6853199)
mean(datNC$voted2010) # NC 2010 (0.4787708) 
mean(datCO$Vote[datCO$Year==2012]) # CO 2012 (0.8280877)
mean(datCO$Vote[datCO$Year==2016]) # CO 2016 (0.8190579)
mean(datNC$Vote[datNC$Year==2012]) # NC 2012 (0.6939296)
mean(datNC$Vote[datNC$Year==2016]) # NC 2016 (0.6236273)

# COMPLETE CASES
mean(!is.na(datCO$Vote[datCO$Year==2012])) # 1 OK
mean(!is.na(datCO$Vote[datCO$Year==2016])) # 1 OK
mean(!is.na(datNC$Vote[datNC$Year==2012])) # 1 OK
mean(!is.na(datNC$Vote[datNC$Year==2016])) # 1 OK


setwd("./NC16Sample")


# SIMPLE RANDOM SAMPLING (All Voters) ==================================================#
set.seed(1029501)
sample_CO <- datCO %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
             sample(size=round(0.01*dim(datCO)[1]), replace=F)   # SAMPLE 1% = 8988
datCO.s <- datCO %>% filter(VoterID %in% sample_CO) %>% arrange(VoterID) # 81566

set.seed(1029501)
sample_NC <- datNC %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
             sample(size=round(0.01*dim(datNC)[1]), replace=F)   # SAMPLE 1% = 24178
datNC.s <- datNC %>% filter(VoterID %in% sample_NC) %>% arrange(VoterID) # 209924

dat_samp <- union_all(datCO.s, datNC.s) # Stack two states again
write_csv(dat_samp, "1_NC2016_Sample.csv")
########################################################################################

# SIMPLE RANDOM SAMPLING (Frequent Voters) ==================================================#
set.seed(1029501)
sample_CO <- datCO %>% filter(voted2010==1) %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.01*dim(datCO)[1]), replace=F)   # SAMPLE 1% = 8988
datCO.s <- datCO %>% filter(VoterID %in% sample_CO) %>% arrange(VoterID)

set.seed(1029501)
sample_NC <- datNC %>% filter(voted2010==1) %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.01*dim(datNC)[1]), replace=F)   # SAMPLE 1% = 24178
datNC.s <- datNC %>% filter(VoterID %in% sample_NC) %>% arrange(VoterID)

dat_samp <- union_all(datCO.s, datNC.s) # Stack two states again
write_csv(dat_samp, "2_NC2016_SampleFreq.csv")
################################################################################################


# SIMPLE RANDOM SAMPLING (Infrequent Voters) ==================================================#
set.seed(1029501)
sample_CO <- datCO %>% filter(voted2010==0) %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.01*dim(datCO)[1]), replace=F)   # SAMPLE 1% = 8988
datCO.s <- datCO %>% filter(VoterID %in% sample_CO) %>% arrange(VoterID)

set.seed(1029501)
sample_NC <- datNC %>% filter(voted2010==0) %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.01*dim(datNC)[1]), replace=F)   # SAMPLE 1% = 24178
datNC.s <- datNC %>% filter(VoterID %in% sample_NC) %>% arrange(VoterID)

dat_samp <- union_all(datCO.s, datNC.s) # Stack two states again
write_csv(dat_samp, "3_NC2016_SampleInfreq.csv")
################################################################################################


# SIMPLE RANDOM SAMPLING (Young less than 35 Voters) ==================================================#
set.seed(1029501)
sample_CO <- datCO %>% filter(age<=40) %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.01*dim(datCO)[1]), replace=F)   # SAMPLE 1% = 8988
datCO.s <- datCO %>% filter(VoterID %in% sample_CO) %>% arrange(VoterID)

set.seed(1029501)
sample_NC <- datNC %>% filter(age<=40) %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.01*dim(datNC)[1]), replace=F)   # SAMPLE 1% = 24178
datNC.s <- datNC %>% filter(VoterID %in% sample_NC) %>% arrange(VoterID)

dat_samp <- union_all(datCO.s, datNC.s) # Stack two states again
write_csv(dat_samp, "4_NC2016_SampleYoung.csv")
################################################################################################



# SIMPLE RANDOM SAMPLING (Middle > 35, < 65  Voters) ==================================================#
set.seed(1029501)
sample_CO <- datCO %>% filter(age>40 & age<=65) %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.01*dim(datCO)[1]), replace=F)   # SAMPLE 1% = 8988
datCO.s <- datCO %>% filter(VoterID %in% sample_CO) %>% arrange(VoterID)

set.seed(1029501)
sample_NC <- datNC %>% filter(age>40 & age<=65) %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.01*dim(datNC)[1]), replace=F)   # SAMPLE 1% = 24178
datNC.s <- datNC %>% filter(VoterID %in% sample_NC) %>% arrange(VoterID)

dat_samp <- union_all(datCO.s, datNC.s) # Stack two states again
write_csv(dat_samp, "5_NC2016_SampleMid.csv")
################################################################################################


# SIMPLE RANDOM SAMPLING (Old over 65 Voters) ==================================================#
set.seed(1029501)
sample_CO <- datCO %>% filter(age>65) %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.01*dim(datCO)[1]), replace=F)   # SAMPLE 1% = 8988
datCO.s <- datCO %>% filter(VoterID %in% sample_CO) %>% arrange(VoterID)

set.seed(1029501)
sample_NC <- datNC %>% filter(age>65) %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.01*dim(datNC)[1]), replace=F)   # SAMPLE 1% = 24178
datNC.s <- datNC %>% filter(VoterID %in% sample_NC) %>% arrange(VoterID)

dat_samp <- union_all(datCO.s, datNC.s) # Stack two states again
write_csv(dat_samp, "6_NC2016_SampleOld.csv")
################################################################################################


# SIMPLE RANDOM SAMPLING (White Voters) ==================================================#
set.seed(1029501)
sample_CO <- datCO %>% filter(estrace=="White") %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.01*dim(datCO)[1]), replace=F)   # SAMPLE 1% = 8988
datCO.s <- datCO %>% filter(VoterID %in% sample_CO) %>% arrange(VoterID)

set.seed(1029501)
sample_NC <- datNC %>% filter(estrace=="White") %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.01*dim(datNC)[1]), replace=F)   # SAMPLE 1% = 24178
datNC.s <- datNC %>% filter(VoterID %in% sample_NC) %>% arrange(VoterID)

dat_samp <- union_all(datCO.s, datNC.s) # Stack two states again
write_csv(dat_samp, "7_NC2016_SampleWhite.csv")
########################################################################################


# SIMPLE RANDOM SAMPLING (Black Voters) ==================================================#
set.seed(1029501)
sample_CO <- datCO %>% filter(estrace=="Black") %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.01*dim(datCO)[1]), replace=F)   # SAMPLE 1% = 8988
datCO.s <- datCO %>% filter(VoterID %in% sample_CO) %>% arrange(VoterID)

set.seed(1029501)
sample_NC <- datNC %>% filter(estrace=="Black") %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.01*dim(datNC)[1]), replace=F)   # SAMPLE 1% = 24178
datNC.s <- datNC %>% filter(VoterID %in% sample_NC) %>% arrange(VoterID)

dat_samp <- union_all(datCO.s, datNC.s) # Stack two states again
write_csv(dat_samp, "8_NC2016_SampleBlack.csv")
########################################################################################


# SIMPLE RANDOM SAMPLING (Hispanic Voters) ==================================================#
set.seed(1029501)
sample_CO <- datCO %>% filter(estrace=="Hispanic") %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.03*dim(datCO)[1]), replace=F)   # SAMPLE 1% = 8988
datCO.s <- datCO %>% filter(VoterID %in% sample_CO) %>% arrange(VoterID)

set.seed(1029501)
sample_NC <- datNC %>% filter(estrace=="Hispanic") %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.003*dim(datNC)[1]), replace=F)   # SAMPLE 1% = 24178
datNC.s <- datNC %>% filter(VoterID %in% sample_NC) %>% arrange(VoterID)

dat_samp <- union_all(datCO.s, datNC.s) # Stack two states again
write_csv(dat_samp, "9_NC2016_SampleHispanic.csv")
################################################################################################


# SIMPLE RANDOM SAMPLING (Asian Voters) ==================================================#
set.seed(1029501)
sample_CO <- datCO %>% filter(estrace=="Asian") %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.02*dim(datCO)[1]), replace=F)   # SAMPLE 1% = 8988
datCO.s <- datCO %>% filter(VoterID %in% sample_CO) %>% arrange(VoterID)

set.seed(1029501)
sample_NC <- datNC %>% filter(estrace=="Asian") %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.005*dim(datNC)[1]), replace=F)   # SAMPLE 1% = 24178
datNC.s <- datNC %>% filter(VoterID %in% sample_NC) %>% arrange(VoterID)

dat_samp <- union_all(datCO.s, datNC.s) # Stack two states again
write_csv(dat_samp, "X10_NC2016_SampleAsian.csv")
################################################################################################

# SIMPLE RANDOM SAMPLING (Female Voters) ==================================================#
set.seed(1029501)
sample_CO <- datCO %>% filter(female==1) %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.01*dim(datCO)[1]), replace=F)   # SAMPLE 1% = 8988
datCO.s <- datCO %>% filter(VoterID %in% sample_CO) %>% arrange(VoterID)

set.seed(1029501)
sample_NC <- datNC %>% filter(female==1) %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.01*dim(datNC)[1]), replace=F)   # SAMPLE 1% = 24178
datNC.s <- datNC %>% filter(VoterID %in% sample_NC) %>% arrange(VoterID)

dat_samp <- union_all(datCO.s, datNC.s) # Stack two states again
write_csv(dat_samp, "X11_NC2016_SampleFemale.csv")
################################################################################################


# SIMPLE RANDOM SAMPLING (Male Voters) ==================================================#
set.seed(1029501)
sample_CO <- datCO %>% filter(female==0) %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.01*dim(datCO)[1]), replace=F)   # SAMPLE 1% = 8988
datCO.s <- datCO %>% filter(VoterID %in% sample_CO) %>% arrange(VoterID)

set.seed(1029501)
sample_NC <- datNC %>% filter(female==0) %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.01*dim(datNC)[1]), replace=F)   # SAMPLE 1% = 24178
datNC.s <- datNC %>% filter(VoterID %in% sample_NC) %>% arrange(VoterID)

dat_samp <- union_all(datCO.s, datNC.s) # Stack two states again
write_csv(dat_samp, "X12_NC2016_SampleMale.csv")
################################################################################################


# SIMPLE RANDOM SAMPLING (Democratic Voters) ==================================================#
set.seed(1029501)
sample_CO <- datCO %>% filter(democrat==1) %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.01*dim(datCO)[1]), replace=F)   # SAMPLE 1% = 8988
datCO.s <- datCO %>% filter(VoterID %in% sample_CO) %>% arrange(VoterID)

set.seed(1029501)
sample_NC <- datNC %>% filter(democrat==1) %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.01*dim(datNC)[1]), replace=F)   # SAMPLE 1% = 24178
datNC.s <- datNC %>% filter(VoterID %in% sample_NC) %>% arrange(VoterID)

dat_samp <- union_all(datCO.s, datNC.s) # Stack two states again
write_csv(dat_samp, "X13_NC2016_SampleDem.csv.csv")
################################################################################################


# SIMPLE RANDOM SAMPLING (Republican Voters) ==================================================#
set.seed(1029501)
sample_CO <- datCO %>% filter(democrat==0) %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.01*dim(datCO)[1]), replace=F)   # SAMPLE 1% = 8988
datCO.s <- datCO %>% filter(VoterID %in% sample_CO) %>% arrange(VoterID)

set.seed(1029501)
sample_NC <- datNC %>% filter(democrat==0) %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.01*dim(datNC)[1]), replace=F)   # SAMPLE 1% = 24178
datNC.s <- datNC %>% filter(VoterID %in% sample_NC) %>% arrange(VoterID)

dat_samp <- union_all(datCO.s, datNC.s) # Stack two states again
write_csv(dat_samp, "X14_NC2016_SampleNonDem.csv")
################################################################################################
