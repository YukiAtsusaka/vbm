################################################################################################
# 03_DID_BVMEffect_NoPreprocessing.R
# Created by Yuki Atsusaka
# Since 12/2/2019
# Last updated 12/2/2019
# Aim: to use the difference-in-differences estimator for the ATE of the VBM policy in Colorado
################################################################################################

################################################################################################
rm(list=ls())
library(tidyverse)
library(magrittr)

################################################################################################
# FOR THE POPULATION OF INTEREST (I)
dat <- read_csv("Stack_Colorado_NC_2012_2016_imputed.csv", col_types = cols(VoterID = col_character()))
dat <- read_csv("Stack_Colorado_NC_2012_2016_imputed_Low.csv", col_types = cols(VoterID = col_character()))
dat <- read_csv("Stack_Colorado_NC_2012_2016_imputed_Up.csv", col_types = cols(VoterID = col_character()))
################################################################################################


# ################################################################################################
# # FOR THE POPULATION OF INTEREST (II)
# dat <- read_csv("Stack_Colorado_NC_2012_2016_expanded.csv", col_types = cols(VoterID = col_character()))
# dat <- dat %>% 
#        filter(is.na(female)==F & is.na(democrat)==F & is.na(age)==F & is.na(estrace)==F) # A lot of people dropped
# ################################################################################################


summary(lm(Vote ~ Intervent +Time+Place+as.factor(estrace)+as.factor(female)+as.factor(democrat)+age+voted2010, dat))$coef[2,1:2]


dat.freq <- dat %>% filter(voted2010==1)
summary(lm(Vote ~ Intervent +Time+Place+as.factor(estrace)+as.factor(democrat)+age+as.factor(female), dat.freq))$coef[2,1:2]


dat.infreq <- dat %>% filter(voted2010==0)
summary(lm(Vote ~ Intervent +Time+Place+as.factor(estrace)+as.factor(democrat)+age+as.factor(female), dat.infreq))$coef[2,1:2]


dat.white <- dat %>% filter(estrace=="White")
summary(lm(Vote ~ Intervent +Time+Place+as.factor(female)+as.factor(democrat)+age+voted2010, dat.white))$coef[2,1:2]


dat.black <- dat %>% filter(estrace=="Black")
summary(lm(Vote ~ Intervent +Time+Place+as.factor(female)+as.factor(democrat)+age+voted2010, dat.black))$coef[2,1:2]


dat.hispanic <- dat %>% filter(estrace=="Hispanic")
summary(lm(Vote ~ Intervent +Time+Place+as.factor(female)+as.factor(democrat)+age+voted2010, dat.hispanic))$coef[2,1:2]


dat.asian <- dat %>% filter(estrace=="Asian")
summary(lm(Vote ~ Intervent +Time+Place+as.factor(female)+as.factor(democrat)+age+voted2010, dat.asian))$coef[2,1:2]


dat.female <- dat %>% filter(female==1)
summary(lm(Vote ~ Intervent +Time+Place+as.factor(estrace)+as.factor(democrat)+age+voted2010, dat.female))$coef[2,1:2]


dat.male <- dat %>% filter(female==0)
summary(lm(Vote ~ Intervent +Time+Place+as.factor(estrace)+as.factor(democrat)+age+voted2010, dat.male))$coef[2,1:2]


dat.dem <- dat %>% filter(democrat==1)
summary(lm(Vote ~ Intervent +Time+Place+as.factor(estrace)+as.factor(female)+age+voted2010, dat.dem))$coef[2,1:2]


dat.rep <- dat %>% filter(democrat==0)
summary(lm(Vote ~ Intervent +Time+Place+as.factor(estrace)+as.factor(female)+age+voted2010, dat.rep))$coef[2,1:2]


dat.agelow <- dat %>% filter(age < 35)
summary(lm(Vote ~ Intervent +Time+Place+as.factor(estrace)+as.factor(female)+as.factor(democrat)+age+voted2010, dat.agelow))$coef[2,1:2]


dat.agemid <- dat %>% filter(age >= 35 & age < 65)
summary(lm(Vote ~ Intervent +Time+Place+as.factor(estrace)+as.factor(female)+as.factor(democrat)+age+voted2010, dat.agemid))$coef[2,1:2]


dat.agehigh <- dat %>% filter(age >= 65)
summary(lm(Vote ~ Intervent +Time+Place+as.factor(estrace)+as.factor(female)+as.factor(democrat)+age+voted2010, dat.agehigh))$coef[2,1:2]




################################################################################################
# END OF THIS R SOURCE CODE
################################################################################################
  
