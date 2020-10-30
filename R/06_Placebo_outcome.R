################################################################################################
# 03_DID_BVMEffect_NoPreprocessing.R
# Created by Yuki Atsusaka
# Since 12/2/2019
# Last updated 12/2/2019
# Aim: to use the difference-in-differences estimator for the ATE of the VBM policy in Colorado
################################################################################################

################################################################################################
rm(list=ls());gc();gc()
library(tidyverse)

setwd("data/Placebo")
fname <- list.files(path = ".", pattern = "*.csv")
nvec <- c("Voted2010", "White", "Black", "Hispanic", "Asian", "Female", "Democrat")
tvec <- c("North Carolina 2012 + 2014",
          "North Carolina 2012 + 2016",
          "New Mexico 2012 + 2014",
          "New Mexico 2012 + 2016")

{pdf("Placebo_outcome.pdf", width=12.5, height=7)
par(mar=c(1,3,3,1), mfrow=c(2,2))


for(i in 1:4){
dt <- read_csv(fname[i], col_types = cols(VoterID = col_character()))
  
dt <- dt %>% 
      mutate(being.white=ifelse(estrace=="White",1,0),
             being.black=ifelse(estrace=="Black",1,0),
             being.hisp=ifelse(estrace=="Hispanic",1,0),
             being.asian=ifelse(estrace=="Asian",1,0))
################################################################################################

# OLS with FALSE OUTCOMES
p1 <- summary(lm(voted2010 ~ Intervent +Time+Place+as.factor(estrace)+female+democrat+age, dt))$coef[2,1:2]
p2 <- summary(lm(being.white ~ Intervent +Time+Place+voted2010+female+democrat+age, dt))$coef[2,1:2]
p3 <- summary(lm(being.black ~ Intervent +Time+Place+voted2010+female+democrat+age, dt))$coef[2,1:2]
p4 <- summary(lm(being.hisp ~ Intervent +Time+Place+voted2010+female+democrat+age, dt))$coef[2,1:2]
p5 <- summary(lm(being.asian ~ Intervent +Time+Place+voted2010+female+democrat+age, dt))$coef[2,1:2]
p6 <- summary(lm(female ~ Intervent +Time+Place+voted2010+as.factor(estrace)+democrat+age, dt))$coef[2,1:2]
p7 <- summary(lm(democrat ~ Intervent +Time+Place+voted2010+female+as.factor(estrace)+age, dt))$coef[2,1:2]

plc <- c(p1[1],p2[1],p3[1],p4[1],p5[1],p6[1],p7[1])
se  <- c(p1[2],p2[2],p3[2],p4[2],p5[2],p6[2],p7[2])


plot(0:8, 0:8, type="n", xlab="", ylab="", xaxt="n", ylim=c(-0.01,0.02))
points(1:7, plc, pch=19)
arrows(y0=plc-1.96*se, y1=plc+1.96*se, 
       x0=1:7, x1=1:7, length=0, angle=0, col="firebrick4") # So small and cannot see
abline(h=0, lty=2)
text(x=1:7, y=0.01, labels=nvec, cex=1.2) 
title(tvec[i], cex.main=2)

}

dev.off()
}  

################################################################################################
# END OF THIS R SOURCE CODE
################################################################################################
  

# CHECK
summary(lm(voted2010 ~ Intervent +Time+Place+as.factor(estrace)+female+democrat+age, dt))



