################################################################################################
# 14_Estimation.R
# Aim: to use the difference-in-differences estimator for the ATE of the VBM policy in Colorado
################################################################################################

################################################################################################
# ESTIMAND: ATT (AVERAGED TREATMENT EFFECT ON THE TREATED)
rm(list=ls());gc(); gc()
library(tidyverse)
library(cobalt)
library(MatchIt)
library(lmtest)
library(sandwich)

setwd("C:/Users/YUKI/Box/FromLaptop/Project/03_ColoradoVBM_BOB/VBM_analysis/NM14Sample")

# CURRENTLY BASED ON IMPUTATINO "LOGIT" 8/10/2020

att <- list()
fname <- list.files(path = ".", pattern = "*.csv")


for(i in 1:14){

dat_s <- read_csv(fname[i], col_types = cols(VoterID = col_character()))
dat_s16 <- dat_s %>% filter(Time==1) # GRAPH THE lATER YEAR ONLLY

# CREATING COVARIATE MATRIX WITH DIFFERENT CONDITIONING VARIAVLES
if(i==1){
variable_names2 <- c("voted2010", "female", "age", "estrace", "democrat")   # ALL DATA
}else if(i %in% 2:3){
variable_names2 <- c("female", "age", "estrace", "democrat")   # FOR FREQUENT VOTER DATA
}else if(i %in% 4:6){
variable_names2 <- c("voted2010","female", "estrace", "democrat")   # FOR AGE SPLIT DATA + ALL DATA
}else if(i %in% 7:10){
variable_names2 <- c("voted2010", "female", "age", "democrat") # FOR RACE SPLIT DATA
}else if(i %in% 11:12){
variable_names2 <- c("voted2010","age", "democrat", "estrace") # FOR GENDER SPLIT DATA
}else {
variable_names2 <- c("voted2010","female", "age", "estrace")   # FOR PARTY SPLIT DATA
}

#print(variable_names2) # CHECK


X.16 <- dat_s16[, variable_names2] # COVARIATE MATRIX FOR TWO-YEAR DATA
m.out16     <- matchit(f.build("Place", X.16), data=dat_s16, method="exact")

# CREATING MATCHED DATA
match.16ID <- match.data(m.out16) %>% dplyr::select(VoterID) %>% pull() # VoterID for MATCHED SAMPLE
w <- match.data(m.out16) %>% dplyr::select(c(VoterID, weights)) # Weight for ATT
match.dat <- dat_s %>% filter(VoterID %in% match.16ID) # MATCHED TWO-YEAR DATA
match.dat <- match.dat %>% left_join(w, by="VoterID")

# COVARIATE BALANCE
#love.plot(m.out16, binary = "std", stats = c("mean.diffs", "ks.statistics"), threshold = .1)
# Save by Porrail (8.00 x 5.00)
################################################################################################



################################################################################################
# (3) ESTIMATE CATTs VIA DIFFERENCE-IN-DIFFERENCES WLS


if(i==1){
# ALL
m <- lm(Vote ~ Intervent +Time+Place+voted2010+as.factor(estrace)+as.factor(female)+as.factor(democrat)+age,match.dat, weights=weights)

}else if(i %in% 2:3){
# Frequent and Infrequent
m <- lm(Vote ~ Intervent +Time+Place+as.factor(estrace)+as.factor(female)+as.factor(democrat)+age, match.dat, weights=weights)

}else if(i %in% 4:6){
# Age Low, Mid, High
m <- lm(Vote ~ Intervent +Time+Place+voted2010+as.factor(estrace)+as.factor(female)+as.factor(democrat), match.dat, weights=weights)


}else if(i %in% 7:10){
# White, Black, Hispanic, Asian
m <- lm(Vote ~ Intervent +Time+Place+voted2010+as.factor(female)+as.factor(democrat)+age, match.dat, weights=weights)
  
}else if(i %in% 11:12){
# Male, Female
m <- lm(Vote ~ Intervent +Time+Place+voted2010+as.factor(estrace)+as.factor(democrat)+age, match.dat, weights=weights)
  
  
}else{
# Dem, non-Dem
m <- lm(Vote ~ Intervent +Time+Place+voted2010+as.factor(estrace)+as.factor(female)+age, match.dat, weights=weights)
}

est <- coeftest(m, vcov = vcovHC(m, type="HC3"))[2,1:2]

att[[i]] <- est
print(c(fname[i], round(est, d=4)))

}

lapply(att, function(x) round(x, d=3))


################################################################################################

################################################################################################
# END OF THIS R SOURCE CODE
################################################################################################
  
