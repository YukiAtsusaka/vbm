################################################################################################
# DID_BVMEffect_Estimation.R
# Created by Yuki Atsusaka
# Since 8/27/2019
# Last updated 8/6/2020
# Aim: to use the difference-in-differences estimator for the ATE of the VBM policy in Colorado
################################################################################################

################################################################################################
################################################################################################
# (1) SIMPLE RANDOM SAMPLING
 rm(list=ls())
 library(tidyverse);gc();gc()
#
 setwd("C:/Users/YUKI/Box/FromLaptop/Project/03_ColoradoVBM_BOB/VBM_analysis")

# LOGIT IMPUTATION
 dt <- read_csv("Stack_Colorado_NC_2012_2016_imputed.csv", col_types = cols(VoterID = col_character())) # PRIMARY POPULATION OF INTEREST
 dt.s <- dt %>% distinct(VoterID, voted2010);rm(dt) # READY TO MERGE
 
 dat <- read_csv("Stack_Colorado_NC_2012_2014.csv", col_types = cols(VoterID = col_character()))
 dat.m <- dat %>% left_join(dt.s, by="VoterID") 
 dat.m <- dat.m %>% mutate(voted2010 = ifelse(!is.na(voted2010.x), voted2010.x, voted2010.y)) %>%
          dplyr::select(-c(voted2010.x, voted2010.y)) %>%
          filter(!is.na(Vote))
write_csv(dat.m, "Stack_Colorado_NC_2012_2014_imputed.csv")  
 
 
# PARTIAL IDENTIFICATINO I
rm(list=ls());gc();gc()
 dt <- read_csv("Stack_Colorado_NC_2012_2016_imputed_Low.csv", col_types = cols(VoterID = col_character())) # PRIMARY POPULATION OF INTEREST
 dt.s <- dt %>% distinct(VoterID, voted2010);rm(dt) # READY TO MERGE
 
 dat <- read_csv("Stack_Colorado_NC_2012_2014.csv", col_types = cols(VoterID = col_character()))
 dat.m <- dat %>% left_join(dt.s, by="VoterID") 
 dat.m <- dat.m %>% mutate(voted2010 = ifelse(!is.na(voted2010.x), voted2010.x, voted2010.y)) %>%
          dplyr::select(-c(voted2010.x, voted2010.y)) %>%
          filter(!is.na(Vote))
write_csv(dat.m, "Stack_Colorado_NC_2012_2014_imputed_Low.csv")  


# PARTIAL IDENTIFICATION II
rm(list=ls());gc();gc()
 dt <- read_csv("Stack_Colorado_NC_2012_2016_imputed_Up.csv", col_types = cols(VoterID = col_character())) # PRIMARY POPULATION OF INTEREST
 dt.s <- dt %>% distinct(VoterID, voted2010);rm(dt) # READY TO MERGE
 
 dat <- read_csv("Stack_Colorado_NC_2012_2014.csv", col_types = cols(VoterID = col_character()))
 dat.m <- dat %>% left_join(dt.s, by="VoterID") 
 dat.m <- dat.m %>% mutate(voted2010 = ifelse(!is.na(voted2010.x), voted2010.x, voted2010.y)) %>%
          dplyr::select(-c(voted2010.x, voted2010.y)) %>%
          filter(!is.na(Vote))
write_csv(dat.m, "Stack_Colorado_NC_2012_2014_imputed_Up.csv")  

################################################################################################
# SIMPLE RANDOM SAMPLE
################################################################################################

rm(list=ls());gc();gc()
library(tidyverse)
setwd("C:/Users/YUKI/Box/FromLaptop/Project/03_ColoradoVBM_BOB/VBM_analysis")
dat <- read_csv("Stack_Colorado_NC_2012_2014_imputed.csv", col_types = cols(VoterID = col_character())) # PRIMARY POPULATION OF INTEREST

datCO <- dat %>% filter(Place==1)
datNC <- dat %>% filter(Place==0)

# SIMPLE RANDOM SAMPLING (All Voters) ==================================================#
set.seed(1029501)
sample_CO <- datCO %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
             sample(size=round(0.01*dim(datCO)[1]), replace=F)   # SAMPLE 1% = 8988
datCO.s <- datCO %>% filter(VoterID %in% sample_CO) %>% arrange(VoterID)

set.seed(1029501)
sample_NC <- datNC %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
             sample(size=round(0.01*dim(datNC)[1]), replace=F)   # SAMPLE 1% = 24178
datNC.s <- datNC %>% filter(VoterID %in% sample_NC) %>% arrange(VoterID)

dat_samp <- union_all(datCO.s, datNC.s) # Stack two states again
write_csv(dat_samp, "Stack_Colorado_NC_2012_2014_Sample.csv")
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
write_csv(dat_samp, "Stack_Colorado_NC_2012_2014_Sample_Frequent.csv")
################################################################################################


# SIMPLE RANDOM SAMPLING (Infrequent Voters) ==================================================#
set.seed(1029501)
sample_CO <- datCO %>% filter(voted2010==0) %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.03*dim(datCO)[1]), replace=F)   # SAMPLE 1% = 8988
datCO.s <- datCO %>% filter(VoterID %in% sample_CO) %>% arrange(VoterID)

set.seed(1029501)
sample_NC <- datNC %>% filter(voted2010==0) %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.03*dim(datNC)[1]), replace=F)   # SAMPLE 1% = 24178
datNC.s <- datNC %>% filter(VoterID %in% sample_NC) %>% arrange(VoterID)

dat_samp <- union_all(datCO.s, datNC.s) # Stack two states again
write_csv(dat_samp, "Stack_Colorado_NC_2012_2014_Sample_Infrequent.csv")
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
write_csv(dat_samp, "Stack_Colorado_NC_2012_2014_Sample_White.csv")
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
write_csv(dat_samp, "Stack_Colorado_NC_2012_2014_Sample_Black.csv")
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
write_csv(dat_samp, "Stack_Colorado_NC_2012_2014_Sample_Hispanic.csv")
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
write_csv(dat_samp, "Stack_Colorado_NC_2012_2014_Sample_Asian.csv")
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
write_csv(dat_samp, "Stack_Colorado_NC_2012_2014_Sample_Male.csv")
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
write_csv(dat_samp, "Stack_Colorado_NC_2012_2014_Sample_Female.csv")
################################################################################################



# SIMPLE RANDOM SAMPLING (Young less than 35 Voters) ==================================================#
set.seed(1029501)
sample_CO <- datCO %>% filter(age<=35) %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.01*dim(datCO)[1]), replace=F)   # SAMPLE 1% = 8988
datCO.s <- datCO %>% filter(VoterID %in% sample_CO) %>% arrange(VoterID)

set.seed(1029501)
sample_NC <- datNC %>% filter(age<=35) %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.01*dim(datNC)[1]), replace=F)   # SAMPLE 1% = 24178
datNC.s <- datNC %>% filter(VoterID %in% sample_NC) %>% arrange(VoterID)

dat_samp <- union_all(datCO.s, datNC.s) # Stack two states again
write_csv(dat_samp, "Stack_Colorado_NC_2012_2014_Sample_AgeLow.csv")
################################################################################################



# SIMPLE RANDOM SAMPLING (Middle > 35, < 65  Voters) ==================================================#
set.seed(1029501)
sample_CO <- datCO %>% filter(age>35 & age<=65) %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.01*dim(datCO)[1]), replace=F)   # SAMPLE 1% = 8988
datCO.s <- datCO %>% filter(VoterID %in% sample_CO) %>% arrange(VoterID)

set.seed(1029501)
sample_NC <- datNC %>% filter(age>35 & age<=65) %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.01*dim(datNC)[1]), replace=F)   # SAMPLE 1% = 24178
datNC.s <- datNC %>% filter(VoterID %in% sample_NC) %>% arrange(VoterID)

dat_samp <- union_all(datCO.s, datNC.s) # Stack two states again
write_csv(dat_samp, "Stack_Colorado_NC_2012_2014_Sample_AgeMid.csv")
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
write_csv(dat_samp, "Stack_Colorado_NC_2012_2014_Sample_AgeHigh.csv")
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
write_csv(dat_samp, "Stack_Colorado_NC_2012_2014_Sample_Democrat.csv")
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
write_csv(dat_samp, "Stack_Colorado_NC_2012_2014_Sample_Republican.csv")
################################################################################################




# SIMPLE RANDOM SAMPLING (THOSE WHO REGISTERED WITHIN 6MONTHS) ==================================================#
set.seed(1029501)
sample_CO <- datCO %>% filter(RGroup==1) %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.03*dim(datCO)[1]), replace=F)   # SAMPLE 1% = 8988
datCO.s <- datCO %>% filter(VoterID %in% sample_CO) %>% arrange(VoterID)

set.seed(1029501)
sample_NC <- datNC %>% filter(RGroup==1) %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.03*dim(datNC)[1]), replace=F)   # SAMPLE 1% = 24178
datNC.s <- datNC %>% filter(VoterID %in% sample_NC) %>% arrange(VoterID)

dat_samp <- union_all(datCO.s, datNC.s) # Stack two states again
write_csv(dat_samp, "Stack_Colorado_NC_2012_2014_Sample_RegSixMonth.csv")
################################################################################################


# SIMPLE RANDOM SAMPLING (THOSE WHO REGISTERED WITHIN 1YEAR, MORE THAN 6MONTHS) ==================================================#
set.seed(1029501)
sample_CO <- datCO %>% filter(RGroup==2) %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.03*dim(datCO)[1]), replace=F)   # SAMPLE 1% = 8988
datCO.s <- datCO %>% filter(VoterID %in% sample_CO) %>% arrange(VoterID)

set.seed(1029501)
sample_NC <- datNC %>% filter(RGroup==2) %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.03*dim(datNC)[1]), replace=F)   # SAMPLE 1% = 24178
datNC.s <- datNC %>% filter(VoterID %in% sample_NC) %>% arrange(VoterID)

dat_samp <- union_all(datCO.s, datNC.s) # Stack two states again
write_csv(dat_samp, "Stack_Colorado_NC_2012_2014_Sample_RegOneYear.csv")
################################################################################################


# SIMPLE RANDOM SAMPLING (THOSE WHO REGISTERED LONGER) ==================================================#
set.seed(1029501)
sample_CO <- datCO %>% filter(RGroup==3) %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.03*dim(datCO)[1]), replace=F)   # SAMPLE 1% = 8988
datCO.s <- datCO %>% filter(VoterID %in% sample_CO) %>% arrange(VoterID)

set.seed(1029501)
sample_NC <- datNC %>% filter(RGroup==3) %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.03*dim(datNC)[1]), replace=F)   # SAMPLE 1% = 24178
datNC.s <- datNC %>% filter(VoterID %in% sample_NC) %>% arrange(VoterID)

dat_samp <- union_all(datCO.s, datNC.s) # Stack two states again
write_csv(dat_samp, "Stack_Colorado_NC_2012_2014_Sample_RegOneYear.csv")
################################################################################################




################################################################################################
# (2) PREPROCESSING DATA
# ESTIMAND: ATT (AVERAGED TREATMENT EFFECT ON THE TREATED)
library(tidyverse)
library(Matching)
library(ebal)
library(cobalt)
library(MatchIt)
library(Hmisc)

rm(list=ls())
# CURRENTLY BASED ON IMPUTATINO "LOGIT" 8/18/2020
dat_s <- read_csv("Stack_Colorado_NC_2012_2014_Sample_Infrequent.csv",     
                  col_types = cols(VoterID = col_character()))

# SUBSETTING DATA FOR TWO DATA TYPE
dat_s16 <- dat_s %>% filter(Time==1)   # For TWO-YEAR DATA

# CREATING COVARIATE MATRIX
variable_names2 <- c("voted2010", "female", "age", "estrace", "democrat")   # ALL DATA
variable_names2 <- c("female", "age", "estrace", "democrat")   # FOR FREQUENT VOTER DATA
variable_names2 <- c("voted2010", "female", "age", "democrat") # FOR RACE SPLIT DATA
variable_names2 <- c("voted2010","age", "democrat", "estrace") # FOR GENDER SPLIT DATA
variable_names2 <- c("voted2010","female", "age", "estrace")   # FOR PARTY SPLIT DATA
variable_names2 <- c("voted2010","female", "age", "estrace", "democrat")   # FOR AGE SPLIT DATA + ALL DATA



X.16 <- dat_s16[, variable_names2] # COVARIATE MATRIX FOR TWO-YEAR DATA
m.out16     <- matchit(f.build("Place", X.16), data=dat_s16, method="exact")

# CREATING MATCHED DATA
match.16ID <- match.data(m.out16) %>% dplyr::select(VoterID) %>% pull() # VoterID for MATCHED SAMPLE
w <- match.data(m.out16) %>% dplyr::select(c(VoterID, weights)) # Weight for ATT
match.dat <- dat_s %>% filter(VoterID %in% match.16ID) # MATCHED TWO-YEAR DATA
match.dat <- match.dat %>% left_join(w, by="VoterID")

#write_csv(match.dat, "Stack_Colorado_NC_2012_2014_Matched.csv")

# COVARIATE BALANCE
love.plot(m.out16, binary = "std", stats = c("mean.diffs", "ks.statistics"), threshold = .1)
# Save by Porrail (8.00 x 5.00)
################################################################################################



################################################################################################
# (3) TWO-YEAR PRE-STRATIFIED AND PREPROCESSED DATA

# ALL
summary(lm(Vote ~ Intervent +Time+Place+voted2010+as.factor(estrace)+as.factor(female)+as.factor(democrat)+age,match.dat, weights=weights))$coef[2,1:2]

# Frequent and Infrequent
summary(lm(Vote ~ Intervent +Time+Place+as.factor(estrace)+as.factor(female)+as.factor(democrat)+age, match.dat, weights=weights))$coef[2,1:2]

# White, Black, Hispanic, Asian
summary(lm(Vote ~ Intervent +Time+Place+voted2010+as.factor(female)+as.factor(democrat)+age, match.dat, weights=weights))#$coef[2,1:2]

# Male, Female
summary(lm(Vote ~ Intervent +Time+Place+voted2010+as.factor(estrace)+as.factor(democrat)+age, match.dat, weights=weights))#$coef[2,1:2]

# Dem, non-Dem
summary(lm(Vote ~ Intervent +Time+Place+voted2010+as.factor(estrace)+as.factor(female)+age, match.dat, weights=weights))#$coef[2,1:2]

# Age Low, Mid, High
summary(lm(Vote ~ Intervent +Time+Place+voted2010+as.factor(estrace)+as.factor(female)+as.factor(democrat)+age, match.dat, weights=weights))#$coef[2,1:2]







################################################################################################


################################################################################################
# (3) TWO-YEAR DATA
# Difference-in-Differences OLS
# WITH COVARIATE
summary(lm(Vote ~ Intervent +Time+Place+as.factor(female)+as.factor(democrat)+age+as.factor(estrace)+voted2010, match.dat))$coef[2,1:2]

# OLS BY RACE
match.dat %>% split(.$estrace) %>%
  map(~ lm(Vote ~ Intervent +Time+Place+as.factor(female)+as.factor(democrat)+age+voted2010, data=.x)) %>% map(summary)

# OLS BY GENDER
match.dat %>% split(.$female) %>%
  map(~ lm(Vote ~ Intervent +Time+Place+as.factor(estrace)+as.factor(democrat)+age+voted2010, data=.x)) %>% map(summary)

# OLS BY AGE
match.dat %>% mutate(AgeGroup = case_when(match.dat$age < 35 ~ "Less than 35",
                                              match.dat$age >= 35 & match.dat$age < 65 ~ "35 to 65",
                                              match.dat$age >= 65 ~ "Over 65")) %>%  split(.$AgeGroup) %>%
  map(~ lm(Vote ~ Intervent +Time+Place+as.factor(estrace)+as.factor(female)+as.factor(democrat)+voted2010, data=.x)) %>% map(summary)

# OLS BY PARTY
match.dat %>% split(.$democrat) %>%
  map(~ lm(Vote ~ Intervent +Time+Place+as.factor(estrace)+as.factor(female)+age+voted2010, data=.x)) %>% map(summary)

################################################################################################








# ################################################################################################################
# # (4) ONE-YEAR DATA
# # (3.1) Difference-in-Means ===================================================================================#
# summary(lm(Vote ~ Place, match.lag.dat))$coef[2,1:2]
# 
# # OLS BY RACE
# match.lag.dat %>% split(.$estrace) %>%
#   map(~ lm(Vote ~ Place, data=.x)) %>% map(summary)
# 
# # OLS BY GENDER
# match.lag.dat %>% split(.$female) %>%
#   map(~ lm(Vote ~ Place, data=.x)) %>% map(summary)
# 
# # OLS BY AGE
# match.lag.dat %>% mutate(AgeGroup = case_when(match.lag.dat$age < 35 ~ "Less than 35",
#                                               match.lag.dat$age >= 35 & match.lag.dat$age < 65 ~ "35 to 65",
#                                               match.lag.dat$age >= 65 ~ "Over 65")) %>%  split(.$AgeGroup) %>%
#   map(~ lm(Vote ~ Place, data=.x)) %>% map(summary)
# 
# # OLS BY PARTY
# match.lag.dat %>% split(.$democrat) %>%
#   map(~ lm(Vote ~ Place, data=.x)) %>% map(summary)
# 
# 
# 
# # (3.2) OLS with Covariates ===================================================================================#
# # WITH COVARIATE
# summary(lm(Vote ~ Place + lag.vote+as.factor(female)+as.factor(democrat)+age+as.factor(estrace), match.lag.dat))$coef[2,1:2]
# 
# # OLS BY RACE
# match.lag.dat %>% split(.$estrace) %>%
#   map(~ lm(Vote ~ Place + lag.vote+as.factor(female)+as.factor(democrat)+age, data=.x)) %>% map(summary)
# 
# # OLS BY GENDER
# match.lag.dat %>% split(.$female) %>%
#   map(~ lm(Vote ~ Place + lag.vote+as.factor(estrace)+as.factor(democrat)+age, data=.x)) %>% map(summary)
# 
# # OLS BY AGE
# match.lag.dat %>% mutate(AgeGroup = case_when(match.lag.dat$age < 35 ~ "Less than 35",
#                                               match.lag.dat$age >= 35 & match.lag.dat$age < 65 ~ "35 to 65",
#                                               match.lag.dat$age >= 65 ~ "Over 65")) %>%  split(.$AgeGroup) %>%
#   map(~ lm(Vote ~ Place + lag.vote+as.factor(estrace)+as.factor(female)+as.factor(democrat), data=.x)) %>% map(summary)
# 
# # OLS BY PARTY
# match.lag.dat %>% split(.$democrat) %>%
#   map(~ lm(Vote ~ Place + lag.vote+as.factor(estrace)+as.factor(female)+age, data=.x)) %>% map(summary)
# 
# 
# 
# # (3.3) First-Difference ======================================================================================#
# # WITH COVARIATE
# summary(lm(I(Vote-lag.vote) ~ Place + lag.vote+as.factor(female)+as.factor(democrat)+age+as.factor(estrace), match.lag.dat))$coef[2,1:2]
# 
# # OLS BY RACE
# match.lag.dat %>% split(.$estrace) %>%
#   map(~ lm(I(Vote-lag.vote) ~ Place +as.factor(female)+as.factor(democrat)+age, data=.x)) %>% map(summary)
# 
# # OLS BY GENDER
# match.lag.dat %>% split(.$female) %>%
#   map(~ lm(I(Vote-lag.vote) ~ Place +as.factor(estrace)+as.factor(democrat)+age, data=.x)) %>% map(summary)
# 
# # OLS BY AGE
# match.lag.dat %>% mutate(AgeGroup = case_when(match.lag.dat$age < 35 ~ "Less than 35",
#                                               match.lag.dat$age >= 35 & match.lag.dat$age < 65 ~ "35 to 65",
#                                               match.lag.dat$age >= 65 ~ "Over 65")) %>%  split(.$AgeGroup) %>%
#   map(~ lm(I(Vote-lag.vote) ~ Place +as.factor(estrace)+as.factor(female)+as.factor(democrat), data=.x)) %>% map(summary)
# 
# # OLS BY PARTY
# match.lag.dat %>% split(.$democrat) %>%
#   map(~ lm(I(Vote-lag.vote) ~ Place +as.factor(estrace)+as.factor(female)+age, data=.x)) %>% map(summary)
# ###############################################################################################################





################################################################################################
# END OF THIS R SOURCE CODE
################################################################################################
  
