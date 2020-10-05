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

dat <- read_csv("Stack_Colorado_NM_2012_2014_imputed.csv", col_types = cols(VoterID = col_character())) # PRIMARY POPULATION OF INTEREST
dat <- dat %>% dplyr::select(-n)
dat$age[dat$Year==2014] <- dat$age[dat$Year==2012] + 2
#dat$Vote[is.na(dat$Vote)] <- 1 # This leads to 0.8386044 (LESS PLAUSIBLE)
dat$Vote[is.na(dat$Vote)] <- 0  # This leads to 0.6461022 (MORE PLAUSIBLE)

datCO <- dat %>% filter(Place==1)
datNM <- dat %>% filter(Place==0)

# TURNOUT BY STATE AND YEAR
mean(datCO$Vote[datCO$Year==2012]) # CO 2012 (0.8300265)
mean(datCO$Vote[datCO$Year==2014]) # CO 2014 (0.6461022) # LOWEST VALUE IMPUTATION
mean(datNM$Vote[datNM$Year==2012]) # NM 2012 (0.6934513)
mean(datNM$Vote[datNM$Year==2014]) # NM 2014 (0.4536366)

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
dat_s <- read_csv("Stack_Colorado_NM_2012_2014_Sample_Republican.csv",     
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


###############################################################################################
# (3) TWO-YEAR PRE-STRATIFIED AND PREPROCESSED DATA

# ALL
summary(lm(Vote ~ Intervent +Time+Place+voted2010+as.factor(estrace)+as.factor(female)+as.factor(democrat)+age,match.dat, weights=weights))$coef[2,1:2]

# Frequent and Infrequent
summary(lm(Vote ~ Intervent +Time+Place+as.factor(estrace)+as.factor(female)+as.factor(democrat)+age, match.dat, weights=weights))$coef[2,1:2]

# Age Low, Mid, High
summary(lm(Vote ~ Intervent +Time+Place+voted2010+as.factor(estrace)+as.factor(female)+as.factor(democrat)+age, match.dat, weights=weights))$coef[2,1:2]


# White, Black, Hispanic, Asian
summary(lm(Vote ~ Intervent +Time+Place+voted2010+as.factor(female)+as.factor(democrat)+age, match.dat, weights=weights))$coef[2,1:2]

# Male, Female
summary(lm(Vote ~ Intervent +Time+Place+voted2010+as.factor(estrace)+as.factor(democrat)+age, match.dat, weights=weights))$coef[2,1:2]

# Dem, non-Dem
summary(lm(Vote ~ Intervent +Time+Place+voted2010+as.factor(estrace)+as.factor(female)+age, match.dat, weights=weights))$coef[2,1:2]


################################################################################################
################################################################################################
# END OF THIS R SOURCE CODE
################################################################################################
  
