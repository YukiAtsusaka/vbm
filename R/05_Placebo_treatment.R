################################################################################################
# 03_DID_BVMEffect_NoPreprocessing.R
# Created by Yuki Atsusaka
# Since 12/2/2019
# Last updated 12/2/2019
# Aim: to use the difference-in-differences estimator for the ATE of the VBM policy in Colorado
################################################################################################

rm(list=ls())
library(tidyverse)
setwd("data/Placebo")

nc14 <- read_csv("1_NC2014_Sample.csv", col_types = cols(VoterID = col_character()))
nc16 <- read_csv("2_NC2016_Sample.csv", col_types = cols(VoterID = col_character()))
nm14 <- read_csv("3_NM2014_Sample.csv", col_types = cols(VoterID = col_character()))
nm16 <- read_csv("4_NM2016_Sample.csv", col_types = cols(VoterID = col_character()))
nc16 <- nc16 %>% dplyr::select(-n)

nc <- union(nc14, nc16)
nm <- union(nm14, nm16)

# CREATING A FALSE TREATMENT VARIABLE: IF VBM adoption happened in...
nc2 <- nc %>% 
       mutate(Placebo1 = ifelse(Year==2016 & State=="Colorado", 1, 0),  
              Placebo2 = ifelse(Year==2016 & State=="North Carolina", 1, 0))
nm2 <- nm %>% 
       mutate(Placebo3 = ifelse(Year==2016 & State=="Colorado", 1, 0),  
              Placebo4 = ifelse(Year==2016 & State=="New Mexico", 1, 0))

placebo1 <- summary(lm(Vote ~ Placebo1 +factor(Year)+Place+voted2010+as.factor(estrace)+female+democrat+age, nc2))$coef[2,1:2]
placebo2 <- summary(lm(Vote ~ Placebo2 +factor(Year)+Place+voted2010+as.factor(estrace)+female+democrat+age, nc2))$coef[2,1:2]
placebo3 <- summary(lm(Vote ~ Placebo3 +factor(Year)+Place+voted2010+as.factor(estrace)+female+democrat+age, nm2))$coef[2,1:2]
placebo4 <- summary(lm(Vote ~ Placebo4 +factor(Year)+Place+voted2010+as.factor(estrace)+female+democrat+age, nm2))$coef[2,1:2]

y <- seq(from=-0.018, to=0.02, length=101)
x <- seq(from=0, to=100, by=1)
mcol <- "firebrick4"

{pdf("Placebo_treatment.pdf", width=7, height=4.5)
par(mar=c(1,3,1,1))
plot(y ~ x, type="n", xlab="", ylab="", xaxt="n")
abline(h=0, lty=2, col="gray60")
points(20*1, placebo1[1], pch=19)
points(20*2, placebo2[1], pch=19)
points(20*3, placebo3[1], pch=19)
points(20*4, placebo4[1], pch=19)
arrows(y0=placebo1[1]-1.96*placebo1[2], y1=placebo1[1]+1.96*placebo1[2], x0=20, x1=20, length=0, angle=0, col=mcol) # So small and cannot see
arrows(y0=placebo2[1]-1.96*placebo2[2], y1=placebo2[1]+1.96*placebo2[2], x0=20*2, x1=20*2, length=0, angle=0, col=mcol)
arrows(y0=placebo3[1]-1.96*placebo3[2], y1=placebo3[1]+1.96*placebo3[2], x0=20*3, x1=20*3, length=0, angle=0, col=mcol)
arrows(y0=placebo4[1]-1.96*placebo4[2], y1=placebo4[1]+1.96*placebo4[2], x0=20*4, x1=20*4, length=0, angle=0, col=mcol) # So small and cannot see
text(20*1, 0.01, labels="CO-2016")
text(20*2, 0.01, labels="NC-2016")
text(20*3, 0.01, labels="CO-2016")
text(20*4, 0.01, labels="NM-2016")
text(20*1.3, 0.017, labels="Using NC as control state \n the false intervention in")
text(20*3.5, 0.017, labels="Using NM as control state \n the false intervention in")
dev.off()}

################################################################################################
# END OF THIS R SOURCE CODE
################################################################################################
  
