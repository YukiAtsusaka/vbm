################################################################################################
# NewMexico.R
# Created by Yuki Atsusaka
# Since 8/5/2020
# Last updated 8/5/2020
# Aim: to use the difference-in-differences estimator for the ATE of the VBM policy in Colorado
################################################################################################

################################################################################################
rm(list=ls())
library(tidyverse)
library(haven)
library(wru)

setwd("C:/Users/YUKI/Box/FromLaptop/Project/03_ColoradoVBM_BOB/VBM_analysis")
# dat <- read_dta("new mexico voter file 2017.dta")
# 
# dat2 <- dat %>% mutate(surname = text_name_last,
#                        voterID = text_registrant_id) %>%
#         dplyr::select(surname, voterID)
# write_csv(dat2, "NewMexico_surname.csv")


df = read_csv("NewMexico_surname.csv")
df$surname = gsub("[\U4E00-\U9FFF\U3000-\U303F]", "", df$surname) # Drop Error-based Chinese Letter

############################################################################
# PREDICT RACE BASED ON SURNAME-ONLY METHOD
out <- predict_race(df, surname.year=2010, surname.only=T) 
############################################################################

 est.c <- out[, 3:7] # ONLY EXTRACT RACE PROBABILITIES TO SPEED UP ALGORITHM
 est.c$highest <- as.numeric(apply(est.c, 1, which.max)) # WHICH COLUMN HAS THE HIGHEST PROBABILITY?
 est.c$race <- ifelse(est.c$highest==1, "White", NA)     # ASSIGN MOST LIKELY RACE
 est.c$race[is.na(est.c$race)==T] <- ifelse(est.c$highest[is.na(est.c$race)==T]==2, "Black", NA)
 est.c$race[is.na(est.c$race)==T] <- ifelse(est.c$highest[is.na(est.c$race)==T]==3, "Hispanic", NA)
 est.c$race[is.na(est.c$race)==T] <- ifelse(est.c$highest[is.na(est.c$race)==T]==4, "Asian", NA)
 est.c$race[is.na(est.c$race)==T] <- ifelse(est.c$highest[is.na(est.c$race)==T]==5, "Others", NA)
 
 hist(est.c$highest, main="Histogram of Estimated Race", xlab="1=White, 2=Black, 3=Hispanic, 4=Asian, 5=Others")  
 table(est.c$race)/dim(df)[1]
 
# CHECK THE DISTRIBUTION
# THE PICTURE LOOKS CONSISTENT WITH COLORADO DEMOGRAPHICS
 df$est.race <- est.c$race # FINALLY, ADD ESTIMATED RACE TO THE ORIGINAL DATA FRAME
 head(df) # CHECK THE LAST COLUMN 
 mean(is.na(df$est.race)==F) # Everyone was coded

 df <- df %>% dplyr::select(voterID, est.race)
 
write_csv(df, "NewMexico_Race.csv") 








################################################################################################
# END OF THIS R SOURCE CODE
################################################################################################
  
