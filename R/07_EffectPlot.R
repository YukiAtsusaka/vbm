################################################################################################
# 03_DID_BVMEffect_NoPreprocessing.R
# Created by Yuki Atsusaka
# Since 12/2/2019
# Last updated 12/2/2019
# Aim: to use the difference-in-differences estimator for the ATE of the VBM policy in Colorado
################################################################################################


################################################################################################
# (1) NORTH CAROLINA CONTROLS
################################################################################################

library(tidyverse)
library("ggsci")
rm(list=ls())

pdf("CATTPlot.pdf", width=10, height=7)
par(mar=c(1,3,1,1), mfrow=c(2,1))

# Compute group proportions
# dat <- read_csv("Stack_Colorado_NC_2012_2016_imputed.csv", col_types = cols(VoterID = col_character())) 
# datCO <- dat %>% filter(Place==1); rm(dat)
# datCO %>% group_by(voted2010) %>% summarise(n = n()) %>% mutate(freq = n / sum(n))
# datCO %>% group_by(estrace) %>% summarise(n = n()) %>% mutate(freq = n / sum(n))
# datCO %>% group_by(female) %>% summarise(n = n()) %>% mutate(freq = n / sum(n))
# datCO %>% group_by(democrat) %>% summarise(n = n()) %>% mutate(freq = n / sum(n))
# datCO %>% mutate(ageGR = ifelse(age < 35, 0, ifelse(age>=65, 2,1))) %>%
#           group_by(ageGR) %>% summarise(n = n()) %>% mutate(freq = n / sum(n))

prop = c(1, 0.848, 0.152,                # All, frequent, infrequent
         0.730, 0.102, 0.105, 0.0536,    # White, black, hispanic, asian
         0.522, 0.478,                   # Female, male
         0.322, 0.678,                   # Democrat, non-Democrat
         0.156, 0.618, 0.226)            # U35, 35-65, O65 

# CATTs from Table D.1 on Online Appendix
att <- c(0.057, 0.081, 0.004, 0.057, 0.058, 0.062, 0.048, 0.044, 0.066, 
         0.046, 0.057, 0.037, 0.046, 0.105)
se  <- c(0.003, 0.003, 0.002, 0.003, 0.003, 0.003, 0.003, 0.003, 0.003, 
         0.003, 0.003, 0.004, 0.003, 0.003)


y <- seq(from=0, to=0.12, length=101)
x <- seq(from=0, to=100, by=1)
ratio <- 100/(14)
cole <- "firebrick4"
pal <- pal_jco()(10)


plot(y ~ x, type="n", xlab="", ylab="", xaxt="n")
#abline(h=att[1], lty=1, col="firebrick4")

rect(-3.5, -0.1, ratio*1.5, 0.15, col=alpha(pal[1], 0.6), lty=0)
rect(ratio*1.5, -0.1, ratio*3.5, 0.15, col=alpha(pal[2], 0.6), lty=0)
rect(ratio*3.5, -0.1, ratio*7.5, 0.15, col=alpha(pal[3], 0.6), lty=0)
rect(ratio*7.5, -0.1, ratio*9.5, 0.15, col=alpha(pal[4], 0.6), lty=0)
rect(ratio*9.5, -0.1, ratio*11.5, 0.15, col=alpha(pal[5], 0.6), lty=0)
rect(ratio*11.5, -0.1, ratio*14.5, 0.15, col=alpha(pal[6], 0.6), lty=0)

abline(h=att[1]+1.96*0.003, lty=2, col="firebrick4")
abline(h=att[1]-1.96*0.003, lty=2, col="firebrick4")
abline(v=ratio*1.5, lty=1, col="gray88")
abline(v=ratio*3.5, lty=1, col="gray88")
abline(v=ratio*7.5, lty=1, col="gray88")
abline(v=ratio*9.5, lty=1, col="gray88")
abline(v=ratio*11.5, lty=1, col="gray88")
abline(h=0)

arrows(y0=att[1]-1.96*se[1], y1=att[1]+1.96*se[1],
       x0=ratio*0.5, x1=ratio*0.5, length=0, angle=0, col=cole, lwd=4)
points(ratio*0.5, att[1], pch=1, cex=4)
for(i in 2:14){ arrows(y0=att[i]-1.96*se[i], y1=att[i]+1.96*se[i],
                       x0=ratio*i, x1=ratio*i, length=0, angle=0, col=cole, lwd=4)}
for(i in 2:14){ points(ratio*i, att[i], pch=1, cex=4*prop[i], col="navy") }

text(x=ratio*1.3,y=0.115, labels="North Carolina as Control", font=2)
text(x=ratio*0.5,y=att[1]-0.01, labels="All Voter", font=2)
text(x=ratio*2, y=att[2]+0.01, labels="Frequent Voter", font=2)
text(x=ratio*3, y=att[3]+0.01, labels="Infrequent Voter", font=2)
text(x=ratio*4, y=att[4]-0.01, labels="White", font=2)
text(x=ratio*5, y=att[5]-0.01, labels="Black", font=2)
text(x=ratio*6, y=att[6]-0.01, labels="Hispanic", font=2)
text(x=ratio*7, y=att[7]-0.01, labels="Asian", font=2)
text(x=ratio*8, y=att[8]+0.01, labels="Female", font=2)
text(x=ratio*9, y=att[9]+0.01, labels="Male", font=2)
text(x=ratio*10, y=att[10]-0.01, labels="Dem", font=2)
text(x=ratio*11, y=att[11]-0.01, labels="Non-Dem", font=2)
text(x=ratio*12, y=att[12]+0.01, labels="U35", font=2)
text(x=ratio*13, y=att[13]+0.01, labels="35-65", font=2)
text(x=ratio*14, y=att[14]+0.01, labels="O65", font=2)


################################################################################################
# (2) NEW MEXICO CONTROLS
################################################################################################


# Compute group proportions
# dat <- read_csv("Stack_Colorado_NC_2012_2016_imputed.csv", col_types = cols(VoterID = col_character()))
# datCO <- dat %>% filter(Place==1); rm(dat)
# datCO %>% group_by(voted2010) %>% summarise(n = n()) %>% mutate(freq = n / sum(n))
# datCO %>% group_by(estrace) %>% summarise(n = n()) %>% mutate(freq = n / sum(n))
# datCO %>% group_by(female) %>% summarise(n = n()) %>% mutate(freq = n / sum(n))
# datCO %>% group_by(democrat) %>% summarise(n = n()) %>% mutate(freq = n / sum(n))
# datCO %>% mutate(ageGR = ifelse(age < 35, 0, ifelse(age>=65, 2,1))) %>%
#           group_by(ageGR) %>% summarise(n = n()) %>% mutate(freq = n / sum(n))

prop = c(1, 0.848, 0.152,                # All, frequent, infrequent
         0.730, 0.102, 0.105, 0.0536,    # White, black, hispanic, asian
         0.522, 0.478,                   # Female, male
         0.322, 0.678,                   # Democrat, non-Democrat
         0.156, 0.618, 0.226)            # U35, 35-65, O65 

# CATTs from Table D.1 on Online Appendix 
att <- c(0.052, 0.079, 0.051, 0.059, 0.063, 0.024, 0.073, 0.052, 0.056, 
         0.037, 0.060, 0.024, 0.060, 0.069)
se  <- c(0.003, 0.002, 0.002, 0.003, 0.006, 0.002, 0.006, 0.003, 0.003, 
         0.003, 0.003, 0.003, 0.003, 0.002)


y <- seq(from=0, to=0.12, length=101)
x <- seq(from=0, to=100, by=1)
ratio <- 100/(14)
cole <- "firebrick4"
pal <- pal_jco()(10)


plot(y ~ x, type="n", xlab="", ylab="", xaxt="n")
#abline(h=att[1], lty=1, col="firebrick4")

rect(-3.5, -0.1, ratio*1.5, 0.15, col=alpha(pal[1], 0.6), lty=0)
rect(ratio*1.5, -0.1, ratio*3.5, 0.15, col=alpha(pal[2], 0.6), lty=0)
rect(ratio*3.5, -0.1, ratio*7.5, 0.15, col=alpha(pal[3], 0.6), lty=0)
rect(ratio*7.5, -0.1, ratio*9.5, 0.15, col=alpha(pal[4], 0.6), lty=0)
rect(ratio*9.5, -0.1, ratio*11.5, 0.15, col=alpha(pal[5], 0.6), lty=0)
rect(ratio*11.5, -0.1, ratio*14.5, 0.15, col=alpha(pal[6], 0.6), lty=0)

abline(h=att[1]+1.96*0.003, lty=2, col="firebrick4")
abline(h=att[1]-1.96*0.003, lty=2, col="firebrick4")
abline(v=ratio*1.5, lty=1, col="gray88")
abline(v=ratio*3.5, lty=1, col="gray88")
abline(v=ratio*7.5, lty=1, col="gray88")
abline(v=ratio*9.5, lty=1, col="gray88")
abline(v=ratio*11.5, lty=1, col="gray88")
abline(h=0)

arrows(y0=att[1]-1.96*se[1], y1=att[1]+1.96*se[1],
       x0=ratio*0.5, x1=ratio*0.5, length=0, angle=0, col=cole, lwd=4)
points(ratio*0.5, att[1], pch=1, cex=4)
for(i in 2:14){ arrows(y0=att[i]-1.96*se[i], y1=att[i]+1.96*se[i],
                       x0=ratio*i, x1=ratio*i, length=0, angle=0, col=cole, lwd=4)}
for(i in 2:14){ points(ratio*i, att[i], pch=1, cex=4*prop[i], col="navy") }

text(x=ratio*1.3,y=0.115, labels="New Mexico as Control", font=2)
text(x=ratio*0.5,y=att[1]-0.01, labels="All Voter", font=2)
text(x=ratio*2, y=att[2]+0.01, labels="Frequent Voter", font=2)
text(x=ratio*3, y=att[3]+0.01, labels="Infrequent Voter", font=2)
text(x=ratio*4, y=att[4]-0.01, labels="White", font=2)
text(x=ratio*5, y=att[5]-0.01, labels="Black", font=2)
text(x=ratio*6, y=att[6]-0.01, labels="Hispanic", font=2)
text(x=ratio*7, y=att[7]-0.01, labels="Asian", font=2)
text(x=ratio*8, y=att[8]+0.01, labels="Female", font=2)
text(x=ratio*9, y=att[9]+0.01, labels="Male", font=2)
text(x=ratio*10, y=att[10]-0.01, labels="Dem", font=2)
text(x=ratio*11, y=att[11]-0.01, labels="Non-Dem", font=2)
text(x=ratio*12, y=att[12]+0.01, labels="U35", font=2)
text(x=ratio*13, y=att[13]+0.01, labels="35-65", font=2)
text(x=ratio*14, y=att[14]+0.01, labels="O65", font=2)

dev.off()






################################################################################################
# END OF THIS R SOURCE CODE
################################################################################################
  

