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

dat <- read_csv("Stack_Colorado_NC_2012_2016.csv", col_types = cols(VoterID = col_character()))
dat <- dat %>% 
        dplyr::select(-n) %>%
        filter(is.na(female)==F & is.na(democrat)==F & is.na(age)==F & is.na(estrace)==F) %>%
        mutate(Time1 = ifelse(Year==2012, 1,0),
               Time2 = ifelse(Year==2012, 1,0),
               Time3 = ifelse(Year==2016, 1,0),
               Place1 = ifelse(State=="Colorado", 1,0),
               Place2 = ifelse(State=="North Carolina", 1,0),
               Place3 = ifelse(State=="North Carolina", 1,0),
               Placebo1 = ifelse(Time1==1 & Place1==1, 1, 0),  # 2012 CO
               Placebo2 = ifelse(Time2==1 & Place2==1, 1, 0),  # 2012 NC
               Placebo3 = ifelse(Time3==1 & Place3==1, 1, 0))  # 2016 NC
################################################################################################


################################################################################################
# FOR THE POPULATION OF INTEREST (II)

dat2 <- read_csv("Stack_Colorado_NC_2012_2016_Sample.csv", col_types = cols(VoterID = col_character()))
dat2 <- dat2 %>% 
        filter(is.na(female)==F & is.na(democrat)==F & is.na(age)==F & is.na(estrace)==F) %>% # A lot of people dropped
        mutate(Time1 = ifelse(Year==2012, 1,0),
               Time2 = ifelse(Year==2012, 1,0),
               Time3 = ifelse(Year==2016, 1,0),
               Place1 = ifelse(State=="Colorado", 1,0),
               Place2 = ifelse(State=="North Carolina", 1,0),
               Place3 = ifelse(State=="North Carolina", 1,0),
               Placebo1 = ifelse(Time1==1 & Place1==1, 1, 0),  # 2012 CO
               Placebo2 = ifelse(Time2==1 & Place2==1, 1, 0),  # 2012 NC
               Placebo3 = ifelse(Time3==1 & Place3==1, 1, 0))  # 2016 NC
################################################################################################


placebo1 <- summary(lm(Vote ~ Placebo1 +Time1+Place1+as.factor(estrace)+as.factor(female)+as.factor(democrat)+age, dat))$coef[2,1:2]
placebo2 <- summary(lm(Vote ~ Placebo2 +Time2+Place2+as.factor(estrace)+as.factor(female)+as.factor(democrat)+age, dat))$coef[2,1:2]
placebo3 <- summary(lm(Vote ~ Placebo3 +Time3+Place3+as.factor(estrace)+as.factor(female)+as.factor(democrat)+age, dat))$coef[2,1:2]
placebo4 <- summary(lm(Vote ~ Placebo1 +Time1+Place1+as.factor(estrace)+as.factor(female)+as.factor(democrat)+age, dat2))$coef[2,1:2]
placebo5 <- summary(lm(Vote ~ Placebo2 +Time2+Place2+as.factor(estrace)+as.factor(female)+as.factor(democrat)+age, dat2))$coef[2,1:2]
placebo6 <- summary(lm(Vote ~ Placebo3 +Time3+Place3+as.factor(estrace)+as.factor(female)+as.factor(democrat)+age, dat2))$coef[2,1:2]



y <- seq(from=-0.2, to=0.2, length=101)
x <- seq(from=0, to=100, by=1)


pdf("Placebo_treatment.pdf", width=8*0.75, height=7*0.75)
par(mar=c(1,3,1,1))
plot(y ~ x, type="n", xlab="", ylab="", xaxt="n")
points(16*1, placebo1[1], pch=19)
points(16*2, placebo2[1], pch=19)
points(16*3, placebo3[1], pch=19)
points(16*4, placebo4[1], pch=19)
points(16*5, placebo5[1], pch=19)
points(16*6, placebo6[1], pch=19)
arrows(y0=placebo1[1]-1.96*placebo1[2], y1=placebo1[1]+1.96*placebo1[2], x0=16, x1=16, length=0, angle=0, col="red") # So small and cannot see
arrows(y0=placebo2[1]-1.96*placebo2[2], y1=placebo2[1]+1.96*placebo2[2], x0=16*2, x1=16*2, length=0, angle=0, col="red")
arrows(y0=placebo3[1]-1.96*placebo3[2], y1=placebo3[1]+1.96*placebo3[2], x0=16*3, x1=16*3, length=0, angle=0, col="red")
arrows(y0=placebo4[1]-1.96*placebo4[2], y1=placebo4[1]+1.96*placebo4[2], x0=16*4, x1=16*4, length=0, angle=0, col="red") # So small and cannot see
arrows(y0=placebo5[1]-1.96*placebo5[2], y1=placebo5[1]+1.96*placebo5[2], x0=16*5, x1=16*5, length=0, angle=0, col="red")
arrows(y0=placebo6[1]-1.96*placebo6[2], y1=placebo6[1]+1.96*placebo6[2], x0=16*6, x1=16*6, length=0, angle=0, col="red")
abline(h=0, lty=2)
text(x=16, y=-0.08, labels="CO2012") 
text(x=16*2, y=0.08, labels="NC2012") 
text(x=16*3, y=-0.08, labels="NC2016") 
text(x=16*4, y=-0.11, labels="CO2012\n(Matched)") 
text(x=16*5, y=0.11, labels="NC2012\n(Matched)") 
text(x=16*6, y=-0.11, labels="NC2016\n(Mathced)") 
text(x=28, y=0.18, labels="Placebo Tests with False Treatments", cex=1.1)

dev.off()

################################################################################################
# END OF THIS R SOURCE CODE
################################################################################################
  
