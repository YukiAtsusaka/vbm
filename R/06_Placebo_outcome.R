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

dat <- read_csv("Stack_Colorado_NC_2012_2016_Imputed.csv", col_types = cols(VoterID = col_character()))
dat <- dat %>% 
       filter(is.na(female)==F & is.na(democrat)==F & is.na(age)==F & is.na(estrace)==F) %>%
       mutate(being.white=ifelse(estrace=="White",1,0))
################################################################################################

################################################################################################
# FOR THE POPULATION OF INTEREST (I) MATCHED
dat2 <- read_csv("Stack_Colorado_NC_2012_2016_Sample.csv", col_types = cols(VoterID = col_character()))
dat2 <- dat2 %>% 
        filter(is.na(female)==F & is.na(democrat)==F & is.na(age)==F & is.na(estrace)==F) %>%
        mutate(being.white=ifelse(estrace=="White",1,0))
################################################################################################


placebo1 <- summary(lm(female ~ Intervent +Time+Place+as.factor(estrace)+as.factor(democrat)+age, dat))$coef[2,1:2]
placebo2 <- summary(lm(democrat ~ Intervent +Time+Place+as.factor(estrace)+as.factor(female)+age, dat))$coef[2,1:2]
placebo3 <- summary(lm(being.white ~ Intervent +Time+Place+as.factor(democrat)+as.factor(female)+age, dat))$coef[2,1:2]
placebo4 <- summary(lm(female ~ Intervent +Time+Place+as.factor(estrace)+as.factor(democrat)+age, dat2))$coef[2,1:2]
placebo5 <- summary(lm(democrat ~ Intervent +Time+Place+as.factor(estrace)+as.factor(female)+age, dat2))$coef[2,1:2]
placebo6 <- summary(lm(being.white ~ Intervent +Time+Place+as.factor(democrat)+as.factor(female)+age, dat2))$coef[2,1:2]



y <- seq(from=-0.05, to=0.05, length=101)
x <- seq(from=0, to=100, by=1)

pdf("Placebo_outcome.pdf", width=8*0.75, height=7*0.75)
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
text(x=16, y=-0.02, labels="Democrat") 
text(x=16*2, y=-0.02, labels="Female") 
text(x=16*3, y=-0.02, labels="White") 
text(x=16*4, y=-0.02, labels="Democrat\n(Matched)") 
text(x=16*5, y=-0.02, labels="Female\n(Matched)") 
text(x=16*6, y=-0.02, labels="White\n(Mathced)") 
text(x=28, y=0.04, labels="Placebo Tests with False Outcomes", cex=1.1)

dev.off()


################################################################################################
# END OF THIS R SOURCE CODE
################################################################################################
  
