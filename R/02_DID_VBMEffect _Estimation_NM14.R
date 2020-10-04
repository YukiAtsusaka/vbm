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
#
 setwd("C:/Users/YUKI/Box/FromLaptop/Project/03_ColoradoVBM_BOB/VBM_analysis")

# LOGIT IMPUTATION

########################################################################################################## 
# In this imputation file, there are mltiple voters who appear twice with different values of voted2010
# 10/3/2020
##########################################################################################################
  
 dt <- read_csv("Stack_Colorado_NM_2012_2016_imputed.csv", col_types = cols(VoterID = col_character())) # PRIMARY POPULATION OF INTEREST
 dt.s <- dt %>% distinct(VoterID, State, voted2010);rm(dt) # READY TO MERGE
 dt.s$VoterID[dt.s$State=="Colorado"] <- paste0("c", dt.s$VoterID[dt.s$State=="Colorado"])  
 dt.s <- dt.s %>% add_count(VoterID)
 dt.s <- dt.s %>% filter(n==1) %>% dplyr::select(-n)
 
 dat <- read_csv("Stack_Colorado_NM_2012_2014.csv", col_types = cols(VoterID = col_character())) # N=6500376
 dat$tag[dat$Year==2014] <- ifelse(is.na(dat$Vote[dat$Year==2014]), 1,0)
 dat <- dat %>% group_by(VoterID,State) %>% 
        tidyr::fill(tag, .direction = "downup") %>% ungroup()

mean(!is.na(dat$tag)) # This must be 1
mean(!is.na(dat$Vote)) # This may not be 1
  dat <- dat %>% filter(tag==0) # N=5635202
 mean(!is.na(dat$Vote)) # This must be 1

  
 dat$VoterID[dat$State=="Colorado"] <- paste0("c", dat$VoterID[dat$State=="Colorado"])  
 dat.m <- dat %>% left_join(dt.s, by="VoterID") 
 dat.m$VoterID[dat.m$State=="Colorado"] <- gsub("[a-zA-Z ]", "",dat.m$VoterID[dat.m$State=="Colorado"])  
 dat.m$age[dat.m$State=="Colorado" & dat.m$Year==2014] <- dat.m$age[dat.m$State=="Colorado" & dat.m$Year==2014] - 2

 dat.m <- dat.m %>% mutate(voted2010 = ifelse(!is.na(voted2010.x), voted2010.x, voted2010.y)) %>%
          dplyr::select(-c(voted2010.x, voted2010.y, n))
write_csv(dat.m, "Stack_Colorado_NM_2012_2014_imputed.csv")  
 
 
# PARTIAL IDENTIFICATINO I
rm(list=ls());gc();gc()
 dt <- read_csv("Stack_Colorado_NM_2012_2016_imputed_Low.csv", col_types = cols(VoterID = col_character())) # PRIMARY POPULATION OF INTEREST
 dt.s <- dt %>% distinct(VoterID, voted2010);rm(dt) # READY TO MERGE
 
 dat <- read_csv("Stack_Colorado_NM_2012_2014.csv", col_types = cols(VoterID = col_character()))
 dat.m <- dat %>% left_join(dt.s, by="VoterID") 
 dat.m <- dat.m %>% mutate(voted2010 = ifelse(!is.na(voted2010.x), voted2010.x, voted2010.y)) %>%
          dplyr::select(-c(voted2010.x, voted2010.y, n)) %>%
          filter(!is.na(Vote))
write_csv(dat.m, "Stack_Colorado_NM_2012_2014_imputed_Low.csv")  


# PARTIAL IDENTIFICATION II
rm(list=ls());gc();gc()
 dt <- read_csv("Stack_Colorado_NM_2012_2016_imputed_Up.csv", col_types = cols(VoterID = col_character())) # PRIMARY POPULATION OF INTEREST
 dt.s <- dt %>% distinct(VoterID, voted2010);rm(dt) # READY TO MERGE
 
 dat <- read_csv("Stack_Colorado_NM_2012_2014.csv", col_types = cols(VoterID = col_character()))
 dat.m <- dat %>% left_join(dt.s, by="VoterID") 
 dat.m <- dat.m %>% mutate(voted2010 = ifelse(!is.na(voted2010.x), voted2010.x, voted2010.y)) %>%
          dplyr::select(-c(voted2010.x, voted2010.y, n)) %>%
          filter(!is.na(Vote))
write_csv(dat.m, "Stack_Colorado_NM_2012_2014_imputed_Up.csv")  


##################################################s##############################################
# SIMPLE RANDOM SAMPLE
##################################################s##############################################

setwd("C:/Users/YUKI/Box/FromLaptop/Project/03_ColoradoVBM_BOB/VBM_analysis")
rm(list=ls()); gc(); gc()
library(tidyverse)

dat <- read_csv("Stack_Colorado_NM_2012_2014_imputed.csv", col_types = cols(VoterID = col_character())) # PRIMARY POPULATION OF INTEREST

datCO <- dat %>% filter(Place==1)
datNM <- dat %>% filter(Place==0)

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
write_csv(dat_samp, "Stack_Colorado_NM_2012_2014_Sample.csv")
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
write_csv(dat_samp, "Stack_Colorado_NM_2012_2014_Sample_Frequent.csv")
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
write_csv(dat_samp, "Stack_Colorado_NM_2012_2014_Sample_Infrequent.csv")
################################################################################################

# SIMPLE RANDOM SAMPLING (Young less than 35 Voters) ==================================================#
set.seed(1029501)
sample_CO <- datCO %>% filter(age<=35) %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.01*dim(datCO)[1]), replace=F)   # SAMPLE 1% = 8988
datCO.s <- datCO %>% filter(VoterID %in% sample_CO) %>% arrange(VoterID)

set.seed(1029501)
sample_NM <- datNM %>% filter(age<=35) %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.1*dim(datNM)[1]), replace=F)   # SAMPLE 1% = 24178
datNM.s <- datNM %>% filter(VoterID %in% sample_NM) %>% arrange(VoterID)

dat_samp <- union_all(datCO.s, datNM.s) # Stack two states again
write_csv(dat_samp, "Stack_Colorado_NM_2012_2014_Sample_AgeLow.csv")
################################################################################################


# SIMPLE RANDOM SAMPLING (Middle > 35, < 65  Voters) ==================================================#
set.seed(1029501)
sample_CO <- datCO %>% filter(age>35 & age<=65) %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.01*dim(datCO)[1]), replace=F)   # SAMPLE 1% = 8988
datCO.s <- datCO %>% filter(VoterID %in% sample_CO) %>% arrange(VoterID)

set.seed(1029501)
sample_NM <- datNM %>% filter(age>35 & age<=65) %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.1*dim(datNM)[1]), replace=F)   # SAMPLE 1% = 24178
datNM.s <- datNM %>% filter(VoterID %in% sample_NM) %>% arrange(VoterID)

dat_samp <- union_all(datCO.s, datNM.s) # Stack two states again
write_csv(dat_samp, "Stack_Colorado_NM_2012_2014_Sample_AgeMid.csv")
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
write_csv(dat_samp, "Stack_Colorado_NM_2012_2014_Sample_AgeHigh.csv")
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
write_csv(dat_samp, "Stack_Colorado_NM_2012_2014_Sample_White.csv")
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
write_csv(dat_samp, "Stack_Colorado_NM_2012_2014_Sample_Black.csv")
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
write_csv(dat_samp, "Stack_Colorado_NM_2012_2014_Sample_Hispanic.csv")
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
write_csv(dat_samp, "Stack_Colorado_NM_2012_2014_Sample_Asian.csv")
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
write_csv(dat_samp, "Stack_Colorado_NM_2012_2014_Sample_Male.csv")
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
write_csv(dat_samp, "Stack_Colorado_NM_2012_2014_Sample_Female.csv")
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
write_csv(dat_samp, "Stack_Colorado_NM_2012_2014_Sample_Democrat.csv")
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
write_csv(dat_samp, "Stack_Colorado_NM_2012_2014_Sample_Republican.csv")
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
rm(list=ls());gc(); gc()

setwd("C:/Users/YUKI/Box/FromLaptop/Project/03_ColoradoVBM_BOB/VBM_analysis")

# CURRENTLY BASED ON IMPUTATION "LOGIT/10/3/2020
dat_s <- read_csv("Stack_Colorado_NM_2012_2014_Sample_Infrequent.csv",     
                  col_types = cols(VoterID = col_character()))

# SUBSETTING DATA FOR TWO DATA TYPE
dat_s16 <- dat_s %>% filter(Time==1)   # For TWO-YEAR DATA

# CREATING COVARIATE MATRIX
variable_names2 <- c("voted2010", "female", "age", "estrace", "democrat")   # ALL DATA
variable_names2 <- c("female", "age", "estrace", "democrat")   # FOR FREQUENT VOTER DATA
variable_names2 <- c("voted2010","female", "age", "estrace", "democrat")   # FOR AGE SPLIT DATA + ALL DATA

variable_names2 <- c("voted2010", "female", "age", "democrat") # FOR RACE SPLIT DATA
variable_names2 <- c("voted2010","age", "democrat", "estrace") # FOR GENDER SPLIT DATA
variable_names2 <- c("voted2010","female", "age", "estrace")   # FOR PARTY SPLIT DATA


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


3620569/(873783  + 3620569)
1029466/(976558 + 1029466)

################################################################################################
# (3) TWO-YEAR PRE-STRATIFIED AND PREPROCESSED DATA

# ALL
summary(lm(Vote ~ Intervent +Time+Place+voted2010+as.factor(estrace)+as.factor(female)+as.factor(democrat)+age,match.dat, weights=weights))$coef[2,1:2]

# Frequent and Infrequent
summary(lm(Vote ~ Intervent +Time+Place+as.factor(estrace)+as.factor(female)+as.factor(democrat)+age, match.dat, weights=weights))$coef[2,1:2]

# White, Black, Hispanic, Asian
summary(lm(Vote ~ Intervent +Time+Place+voted2010+as.factor(female)+as.factor(democrat)+age, match.dat, weights=weights))$coef[2,1:2]

# Male, Female
summary(lm(Vote ~ Intervent +Time+Place+voted2010+as.factor(estrace)+as.factor(democrat)+age, match.dat, weights=weights))$coef[2,1:2]

# Dem, non-Dem
summary(lm(Vote ~ Intervent +Time+Place+voted2010+as.factor(estrace)+as.factor(female)+age, match.dat, weights=weights))$coef[2,1:2]

# Age Low, Mid, High
summary(lm(Vote ~ Intervent +Time+Place+voted2010+as.factor(estrace)+as.factor(female)+as.factor(democrat)+age, match.dat, weights=weights))$coef[2,1:2]




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
  
