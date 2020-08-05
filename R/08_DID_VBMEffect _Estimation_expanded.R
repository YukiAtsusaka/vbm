################################################################################################
# DID_BVMEffect.R
# Created by Yuki Atsusaka
# Since 8/27/2019
# Last updated 8/30/2019
# Aim: to use the difference-in-differences estimator for the ATE of the VBM policy in Colorado
################################################################################################

################################################################################################
# (1) SIMPLE RANDOM SAMPLING
rm(list=ls())
library(tidyverse)
library(magrittr)

ssize <- 20000  # SAMPLE SIZE FOR TREATED UNITS (IN EACH YEAR)
ssize2 <- 60000 # SAMPLE SIZE FOR CONTROL UNITS (IN EACH YEAR)

dat <- read_csv("Stack_Colorado_NC_2012_2016_expanded.csv", col_types = cols(VoterID = col_character())) # PRIMARY POPULATION OF INTEREST
dat <- dat %>% 
       filter(is.na(female)==F & is.na(democrat)==F & is.na(age)==F & is.na(estrace)==F) %>%
       dplyr::select(-estrace.y)# A lot of people dropped

# SEE THE AGGREGTE TURNOUT
dat %>% group_by(State, Year) %>% summarise(Turnout = mean(Vote))
datCO <- dat %>% filter(Place==1) # Colorado Voters
datNC <- dat %>% filter(Place==0) # North Carolina Voters


# SIMPLE RANDOM SAMPLING (All Voters) ==================================================#
set.seed(1029501)
sample_CO <- datCO %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
             sample(size=round(0.001*dim(datCO)[1]), replace=F)   # SAMPLE 1% = 8988
datCO.s <- datCO %>% filter(VoterID %in% sample_CO) %>% arrange(VoterID)

set.seed(1029501)
sample_NC <- datNC %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
             sample(size=round(0.001*dim(datNC)[1]), replace=F)   # SAMPLE 1% = 24178
datNC.s <- datNC %>% filter(VoterID %in% sample_NC) %>% arrange(VoterID)

dat_samp <- union_all(datCO.s, datNC.s) # Stack two states again
write_csv(dat_samp, "Stack_Colorado_NC_2012_2016_Sample_expanded.csv")
########################################################################################


# SIMPLE RANDOM SAMPLING (White Voters) ==================================================#
set.seed(1029501)
sample_CO <- datCO %>% filter(estrace=="White") %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.001*dim(datCO)[1]), replace=F)   # SAMPLE 1% = 8988
datCO.s <- datCO %>% filter(VoterID %in% sample_CO) %>% arrange(VoterID)

set.seed(1029501)
sample_NC <- datNC %>% filter(estrace=="White") %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.001*dim(datNC)[1]), replace=F)   # SAMPLE 1% = 24178
datNC.s <- datNC %>% filter(VoterID %in% sample_NC) %>% arrange(VoterID)

dat_samp <- union_all(datCO.s, datNC.s) # Stack two states again
write_csv(dat_samp, "Stack_Colorado_NC_2012_2016_Sample_White_expanded.csv")
########################################################################################


# SIMPLE RANDOM SAMPLING (Black Voters) ==================================================#
set.seed(1029501)
sample_CO <- datCO %>% filter(estrace=="Black") %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.001*dim(datCO)[1]), replace=F)   # SAMPLE 1% = 8988
datCO.s <- datCO %>% filter(VoterID %in% sample_CO) %>% arrange(VoterID)

set.seed(1029501)
sample_NC <- datNC %>% filter(estrace=="Black") %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.001*dim(datNC)[1]), replace=F)   # SAMPLE 1% = 24178
datNC.s <- datNC %>% filter(VoterID %in% sample_NC) %>% arrange(VoterID)

dat_samp <- union_all(datCO.s, datNC.s) # Stack two states again
write_csv(dat_samp, "Stack_Colorado_NC_2012_2016_Sample_Black_expanded.csv")
########################################################################################


# SIMPLE RANDOM SAMPLING (Hispanic Voters) ==================================================#
set.seed(1029501)
sample_CO <- datCO %>% filter(estrace=="Hispanic") %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.001*dim(datCO)[1]), replace=F)   # SAMPLE 1% = 8988
datCO.s <- datCO %>% filter(VoterID %in% sample_CO) %>% arrange(VoterID)

set.seed(1029501)
sample_NC <- datNC %>% filter(estrace=="Hispanic") %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.001*dim(datNC)[1]), replace=F)   # SAMPLE 1% = 24178
datNC.s <- datNC %>% filter(VoterID %in% sample_NC) %>% arrange(VoterID)

dat_samp <- union_all(datCO.s, datNC.s) # Stack two states again
write_csv(dat_samp, "Stack_Colorado_NC_2012_2016_Sample_Hispanic_expanded.csv")
################################################################################################


# SIMPLE RANDOM SAMPLING (Asian Voters) ==================================================#
set.seed(1029501)
sample_CO <- datCO %>% filter(estrace=="Asian") %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.001*dim(datCO)[1]), replace=F)   # SAMPLE 1% = 8988
datCO.s <- datCO %>% filter(VoterID %in% sample_CO) %>% arrange(VoterID)

set.seed(1029501)
sample_NC <- datNC %>% filter(estrace=="Asian") %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.001*dim(datNC)[1]), replace=F)   # SAMPLE 1% = 24178
datNC.s <- datNC %>% filter(VoterID %in% sample_NC) %>% arrange(VoterID)

dat_samp <- union_all(datCO.s, datNC.s) # Stack two states again
write_csv(dat_samp, "Stack_Colorado_NC_2012_2016_Sample_Asian_expanded.csv")
################################################################################################


# SIMPLE RANDOM SAMPLING (Male Voters) ==================================================#
set.seed(1029501)
sample_CO <- datCO %>% filter(female==0) %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.001*dim(datCO)[1]), replace=F)   # SAMPLE 1% = 8988
datCO.s <- datCO %>% filter(VoterID %in% sample_CO) %>% arrange(VoterID)

set.seed(1029501)
sample_NC <- datNC %>% filter(female==0) %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.001*dim(datNC)[1]), replace=F)   # SAMPLE 1% = 24178
datNC.s <- datNC %>% filter(VoterID %in% sample_NC) %>% arrange(VoterID)

dat_samp <- union_all(datCO.s, datNC.s) # Stack two states again
write_csv(dat_samp, "Stack_Colorado_NC_2012_2016_Sample_Male_expanded.csv")
################################################################################################


# SIMPLE RANDOM SAMPLING (Female Voters) ==================================================#
set.seed(1029501)
sample_CO <- datCO %>% filter(female==1) %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.001*dim(datCO)[1]), replace=F)   # SAMPLE 1% = 8988
datCO.s <- datCO %>% filter(VoterID %in% sample_CO) %>% arrange(VoterID)

set.seed(1029501)
sample_NC <- datNC %>% filter(female==1) %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.001*dim(datNC)[1]), replace=F)   # SAMPLE 1% = 24178
datNC.s <- datNC %>% filter(VoterID %in% sample_NC) %>% arrange(VoterID)

dat_samp <- union_all(datCO.s, datNC.s) # Stack two states again
write_csv(dat_samp, "Stack_Colorado_NC_2012_2016_Sample_Female_expanded.csv")
################################################################################################



# SIMPLE RANDOM SAMPLING (Young less than 35 Voters) ==================================================#
set.seed(1029501)
sample_CO <- datCO %>% filter(age<=35) %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.001*dim(datCO)[1]), replace=F)   # SAMPLE 1% = 8988
datCO.s <- datCO %>% filter(VoterID %in% sample_CO) %>% arrange(VoterID)

set.seed(1029501)
sample_NC <- datNC %>% filter(age<=35) %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.001*dim(datNC)[1]), replace=F)   # SAMPLE 1% = 24178
datNC.s <- datNC %>% filter(VoterID %in% sample_NC) %>% arrange(VoterID)

dat_samp <- union_all(datCO.s, datNC.s) # Stack two states again
write_csv(dat_samp, "Stack_Colorado_NC_2012_2016_Sample_AgeLow_expanded.csv")
################################################################################################



# SIMPLE RANDOM SAMPLING (Middle > 35, < 65  Voters) ==================================================#
set.seed(1029501)
sample_CO <- datCO %>% filter(age>35 & age<=65) %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.001*dim(datCO)[1]), replace=F)   # SAMPLE 1% = 8988
datCO.s <- datCO %>% filter(VoterID %in% sample_CO) %>% arrange(VoterID)

set.seed(1029501)
sample_NC <- datNC %>% filter(age>35 & age<=65) %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.001*dim(datNC)[1]), replace=F)   # SAMPLE 1% = 24178
datNC.s <- datNC %>% filter(VoterID %in% sample_NC) %>% arrange(VoterID)

dat_samp <- union_all(datCO.s, datNC.s) # Stack two states again
write_csv(dat_samp, "Stack_Colorado_NC_2012_2016_Sample_AgeMid_expanded.csv")
################################################################################################


# SIMPLE RANDOM SAMPLING (Old over 65 Voters) ==================================================#
set.seed(1029501)
sample_CO <- datCO %>% filter(age>65) %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.001*dim(datCO)[1]), replace=F)   # SAMPLE 1% = 8988
datCO.s <- datCO %>% filter(VoterID %in% sample_CO) %>% arrange(VoterID)

set.seed(1029501)
sample_NC <- datNC %>% filter(age>65) %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.001*dim(datNC)[1]), replace=F)   # SAMPLE 1% = 24178
datNC.s <- datNC %>% filter(VoterID %in% sample_NC) %>% arrange(VoterID)

dat_samp <- union_all(datCO.s, datNC.s) # Stack two states again
write_csv(dat_samp, "Stack_Colorado_NC_2012_2016_Sample_AgeHigh_expanded.csv")
################################################################################################



# SIMPLE RANDOM SAMPLING (Democratic Voters) ==================================================#
set.seed(1029501)
sample_CO <- datCO %>% filter(democrat==1) %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.001*dim(datCO)[1]), replace=F)   # SAMPLE 1% = 8988
datCO.s <- datCO %>% filter(VoterID %in% sample_CO) %>% arrange(VoterID)

set.seed(1029501)
sample_NC <- datNC %>% filter(democrat==1) %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.001*dim(datNC)[1]), replace=F)   # SAMPLE 1% = 24178
datNC.s <- datNC %>% filter(VoterID %in% sample_NC) %>% arrange(VoterID)

dat_samp <- union_all(datCO.s, datNC.s) # Stack two states again
write_csv(dat_samp, "Stack_Colorado_NC_2012_2016_Sample_Democrat_expanded.csv")
################################################################################################


# SIMPLE RANDOM SAMPLING (Republican Voters) ==================================================#
set.seed(1029501)
sample_CO <- datCO %>% filter(democrat==0) %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.001*dim(datCO)[1]), replace=F)   # SAMPLE 1% = 8988
datCO.s <- datCO %>% filter(VoterID %in% sample_CO) %>% arrange(VoterID)

set.seed(1029501)
sample_NC <- datNC %>% filter(democrat==0) %>% dplyr::select(VoterID) %>% distinct(VoterID) %>% pull() %>%
  sample(size=round(0.001*dim(datNC)[1]), replace=F)   # SAMPLE 1% = 24178
datNC.s <- datNC %>% filter(VoterID %in% sample_NC) %>% arrange(VoterID)

dat_samp <- union_all(datCO.s, datNC.s) # Stack two states again
write_csv(dat_samp, "Stack_Colorado_NC_2012_2016_Sample_Republican_expanded.csv")
################################################################################################




################################################################################################
# (2) PREPROCESSING DATA
# ESTIMAND: ATT (AVERAGED TREATMENT EFFECT ON THE TREATED)

# (2.1) TWO-YEAR DATA: WE MATCHED ON 2016 VOTERS, KEEP THE SAME VOTERS IN 2012 --> DID-OLS, + Covariates
# (2.2) ONE-YEAR DATA: WE MATCHED ON 2016 VOTEDS WITH 2012 VOTE VARIABLE --> Diff-in-Means, OLS

library(tidyverse)
library(Matching)
library(ebal)
library(cobalt)
library(MatchIt)
library(Hmisc)
rm(list=ls())
dat_s <- read_csv("Stack_Colorado_NC_2012_2016_Sample_expanded.csv",
                  col_types = cols(VoterID = col_character()))
dat_s <- dat_s %>% dplyr::select(-c(voted2010, voted2012))
dat_s <- dat_s %>% group_by(VoterID) %>% mutate(lag.vote = Lag(Vote)) %>% ungroup() # Lagged Outcome for 2016

# SUBSETTING DATA FOR TWO DATA TYPE
#dat_lag <- dat_s %>% filter(Time==1)                               # For ONE-YEAR DATA
dat_s16 <- dat_s %>% filter(Time==1) %>% dplyr::select(-lag.vote)  # For TWO-YEAR DATA

# CREATING COVARIATE MATRIX
variable_names <- c("female", "age", "democrat")
variable_names2 <- c("female", "age", "democrat") # FOR RACE SPLIT DATA
variable_names2 <- c("age", "democrat", "estrace") # FOR GENDER SPLIT DATA
variable_names2 <- c("female", "age", "estrace")   # FOR PARTY SPLIT DATA
variable_names2 <- c("female", "age", "estrace", "democrat")   # FOR AGE SPLIT DATA


#X.lag = dat_lag[, variable_names] # COVARIATE MATRIX FOR ONE-YEAR DATA
X.16 <- dat_s16[, variable_names] # COVARIATE MATRIX FOR TWO-YEAR DATA
# lag.treat <- dat_lag$Place
# lag.outcome <- dat_lag$Vote

# match.lag = Match(Y=lag.outcome, Tr=lag.treat, X=X.lag, M=1, exact=TRUE, estimand="ATT")
# 
# summary(match.lag)

# MatchIT
#m.out16_lag <- matchit(f.build("Place", X.lag), data=dat_lag, exact=TRUE, replace = TRUE)
m.out16     <- matchit(f.build("Place", X.16), data=dat_s16, method="exact")

# CREATING MATCHED DATA
#match.lag.dat <- match.data(m.out16_lag)               # MATCHED ONE-YEAR DATA
match.16ID <- match.data(m.out16) %>% dplyr::select(VoterID) %>% pull() # VoterID for MATCHED SAMPLE
match.dat <- dat_s %>% filter(VoterID %in% match.16ID) # MATCHED TWO-YEAR DATA

# COVARIATE BALANCE
#love.plot(m.out16_lag, binary = "std", stats = c("mean.diffs", "ks.statistics"), threshold = .1)
love.plot(m.out16, binary = "std", stats = c("mean.diffs", "ks.statistics"), threshold = .1)


match.dat.white <- match.dat
match.dat.black <- match.dat
match.dat.hispanic <- match.dat
match.dat.asian <- match.dat
match.dat.male <- match.dat
match.dat.female <- match.dat
match.dat.dem <- match.dat
match.dat.rep <- match.dat
match.dat.agelow <- match.dat
match.dat.agemid <- match.dat
match.dat.agehigh <- match.dat
################################################################################################



################################################################################################
# (3) TWO-YEAR PRE-STRATIFIED AND PREPROCESSED DATA
summary(lm(Vote ~ Intervent +Time+Place+as.factor(female)+as.factor(democrat)+age, match.dat.white))$coef[2,1:2]
summary(lm(Vote ~ Intervent +Time+Place+as.factor(female)+as.factor(democrat)+age, match.dat.black))$coef[2,1:2]
summary(lm(Vote ~ Intervent +Time+Place+as.factor(female)+as.factor(democrat)+age, match.dat.hispanic))$coef[2,1:2]
summary(lm(Vote ~ Intervent +Time+Place+as.factor(female)+as.factor(democrat)+age, match.dat.asian))$coef[2,1:2]
summary(lm(Vote ~ Intervent +Time+Place+as.factor(estrace)+as.factor(democrat)+age, match.dat.male))$coef[2,1:2]
summary(lm(Vote ~ Intervent +Time+Place+as.factor(estrace)+as.factor(democrat)+age, match.dat.female))$coef[2,1:2]
summary(lm(Vote ~ Intervent +Time+Place+as.factor(estrace)+as.factor(female)+age, match.dat.dem))$coef[2,1:2]
summary(lm(Vote ~ Intervent +Time+Place+as.factor(estrace)+as.factor(female)+age, match.dat.rep))$coef[2,1:2]
summary(lm(Vote ~ Intervent +Time+Place+as.factor(estrace)+as.factor(female)+as.factor(democrat)+age, match.dat.agelow))$coef[2,1:2]
summary(lm(Vote ~ Intervent +Time+Place+as.factor(estrace)+as.factor(female)+as.factor(democrat)+age, match.dat.agemid))$coef[2,1:2]
summary(lm(Vote ~ Intervent +Time+Place+as.factor(estrace)+as.factor(female)+as.factor(democrat)+age, match.dat.agehigh))$coef[2,1:2]






################################################################################################


################################################################################################
# (3) TWO-YEAR DATA
# Difference-in-Differences OLS
# WITH COVARIATE
summary(lm(Vote ~ Intervent +Time+Place+as.factor(female)+as.factor(democrat)+age+as.factor(estrace), match.dat))$coef[2,1:2]

# OLS BY RACE
match.dat %>% split(.$estrace) %>%
  map(~ lm(Vote ~ Intervent +Time+Place+as.factor(female)+as.factor(democrat)+age, data=.x)) %>% map(summary)

# OLS BY GENDER
match.dat %>% split(.$female) %>%
  map(~ lm(Vote ~ Intervent +Time+Place+as.factor(estrace)+as.factor(democrat)+age, data=.x)) %>% map(summary)

# OLS BY AGE
match.dat %>% mutate(AgeGroup = case_when(match.dat$age < 35 ~ "Less than 35",
                                              match.dat$age >= 35 & match.dat$age < 65 ~ "35 to 65",
                                              match.dat$age >= 65 ~ "Over 65")) %>%  split(.$AgeGroup) %>%
  map(~ lm(Vote ~ Intervent +Time+Place+as.factor(estrace)+as.factor(female)+as.factor(democrat), data=.x)) %>% map(summary)

# OLS BY PARTY
match.dat %>% split(.$democrat) %>%
  map(~ lm(Vote ~ Intervent +Time+Place+as.factor(estrace)+as.factor(female)+age, data=.x)) %>% map(summary)

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
  
