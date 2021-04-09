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
#setwd("C:/Users/YUKI/Box/FromLaptop/Project/03_ColoradoVBM_BOB/VBM_analysis")
library(tidyverse)
library("ggsci")
rm(list=ls())
setwd("R")

{pdf("CATTPlot_2014.pdf", width=10, height=7)
par(mar=c(1,3,1,1), mfrow=c(2,1))


# #Compute group proportions
# dat <- read_csv("Stack_Colorado_NM_2012_2016.csv", col_types = cols(VoterID = col_character()))
# datCO <- dat %>% filter(Place==1); rm(dat)
# datCO %>% group_by(voted2010) %>% summarise(n = n()) %>% mutate(freq = n / sum(n))
# datCO %>% mutate(ageGR = ifelse(age <= 40, 0, ifelse(age>=65, 2,1))) %>%
#           group_by(ageGR) %>% summarise(n = n()) %>% mutate(freq = n / sum(n))
# datCO %>% group_by(estrace) %>% summarise(n = n()) %>% mutate(freq = n / sum(n))
# datCO %>% group_by(female) %>% summarise(n = n()) %>% mutate(freq = n / sum(n))
# datCO %>% group_by(democrat) %>% summarise(n = n()) %>% mutate(freq = n / sum(n))


prop = c(1, 0.685, 0.315,                # All, frequent, infrequent
         0.240, 0.521, 0.239,            # U35, 35-65, O65 
         0.730, 0.102, 0.105, 0.054,    # White, black, hispanic, asian
         0.523, 0.477,                   # Female, male
         0.323, 0.677)                   # Democrat, non-Democrat

prop <- sqrt(prop)


est <- read_csv("ATT_NC14.csv")
att <- est[,1] %>% pull()
se <- est[,2] %>% pull()



y <- seq(from=0.03, to=0.14, length=101)
x <- seq(from=0, to=100, by=1)
ratio <- 100/(14)
cole <- "firebrick4"
pal <- pal_jco()(10)


plot(y ~ x, type="n", xlab="", ylab="", xaxt="n")
#abline(h=att[1], lty=1, col="firebrick4")

rect(-3.5, -0.1, ratio*1.5, 0.15, col=alpha(pal[1], 0.6), lty=0)
rect(ratio*1.5, -0.1, ratio*3.5, 0.15, col=alpha(pal[2], 0.6), lty=0)
rect(ratio*3.5, -0.1, ratio*6.5, 0.15, col=alpha(pal[3], 0.6), lty=0)
rect(ratio*6.5, -0.1, ratio*10.5, 0.15, col=alpha(pal[4], 0.6), lty=0)
rect(ratio*10.5, -0.1, ratio*12.5, 0.15, col=alpha(pal[5], 0.6), lty=0)
rect(ratio*12.5, -0.1, ratio*14.5, 0.15, col=alpha(pal[6], 0.6), lty=0)

abline(h=att[1]+1.96*0.003, lty=2, col="firebrick4")
abline(h=att[1]-1.96*0.003, lty=2, col="firebrick4")
abline(v=ratio*1.5, lty=1, col="gray88")
abline(v=ratio*3.5, lty=1, col="gray88")
abline(v=ratio*6.5, lty=1, col="gray88")
abline(v=ratio*10.5, lty=1, col="gray88")
abline(v=ratio*12.5, lty=1, col="gray88")
abline(h=0)

arrows(y0=att[1]-1.96*se[1], y1=att[1]+1.96*se[1],
       x0=ratio*0.5, x1=ratio*0.5, length=0, angle=0, col=cole, lwd=4)
points(ratio*0.5, att[1], pch=1, cex=4)
for(i in 2:14){ arrows(y0=att[i]-1.96*se[i], y1=att[i]+1.96*se[i],
                       x0=ratio*i, x1=ratio*i, length=0, angle=0, col=cole, lwd=4)}
for(i in 2:14){ points(ratio*i, att[i], pch=1, cex=4*prop[i], col="navy") }

text(x=ratio*2.8,y=0.135, labels="North Carolina as Control (2014)", font=2, cex=1.5)
text(x=ratio*0.5,y=att[1]-0.01, labels="All Voter", font=2)
text(x=ratio*2, y=att[2]+0.012, labels="Frequent Voter", font=2)
text(x=ratio*3, y=att[3]+0.022, labels="Infrequent Voter", font=2)
text(x=ratio*4, y=att[4]-0.01, labels="U40", font=2)
text(x=ratio*5, y=att[5]-0.01, labels="41-65", font=2)
text(x=ratio*6, y=att[6]-0.01, labels="O65", font=2)
text(x=ratio*7, y=att[7]+0.01, labels="White", font=2)
text(x=ratio*8, y=att[8]+0.01, labels="Black", font=2)
text(x=ratio*9, y=att[9]+0.01, labels="Hispanic", font=2)
text(x=ratio*10, y=att[10]+0.01, labels="Asian", font=2)
text(x=ratio*11, y=att[11]-0.01, labels="Female", font=2)
text(x=ratio*12, y=att[12]-0.01, labels="Male", font=2)
text(x=ratio*13, y=att[13]+0.01, labels="Dem", font=2)
text(x=ratio*14-1, y=att[14]+0.01, labels="Non-Dem", font=2)


################################################################################################
# (2) NEW MEXICO CONTROLS
################################################################################################

est <- read_csv("ATT_NM14.csv")
att <- est[,1] %>% pull()
se <- est[,2] %>% pull()

plot(y ~ x, type="n", xlab="", ylab="", xaxt="n")
#abline(h=att[1], lty=1, col="firebrick4")

rect(-3.5, -0.1, ratio*1.5, 0.15, col=alpha(pal[1], 0.6), lty=0)
rect(ratio*1.5, -0.1, ratio*3.5, 0.15, col=alpha(pal[2], 0.6), lty=0)
rect(ratio*3.5, -0.1, ratio*6.5, 0.15, col=alpha(pal[3], 0.6), lty=0)
rect(ratio*6.5, -0.1, ratio*10.5, 0.15, col=alpha(pal[4], 0.6), lty=0)
rect(ratio*10.5, -0.1, ratio*12.5, 0.15, col=alpha(pal[5], 0.6), lty=0)
rect(ratio*12.5, -0.1, ratio*14.5, 0.15, col=alpha(pal[6], 0.6), lty=0)

abline(h=att[1]+1.96*0.003, lty=2, col="firebrick4")
abline(h=att[1]-1.96*0.003, lty=2, col="firebrick4")
abline(v=ratio*1.5, lty=1, col="gray88")
abline(v=ratio*3.5, lty=1, col="gray88")
abline(v=ratio*6.5, lty=1, col="gray88")
abline(v=ratio*10.5, lty=1, col="gray88")
abline(v=ratio*12.5, lty=1, col="gray88")
abline(h=0)

arrows(y0=att[1]-1.96*se[1], y1=att[1]+1.96*se[1],
       x0=ratio*0.5, x1=ratio*0.5, length=0, angle=0, col=cole, lwd=4)
points(ratio*0.5, att[1], pch=1, cex=4)
for(i in 2:14){ arrows(y0=att[i]-1.96*se[i], y1=att[i]+1.96*se[i],
                       x0=ratio*i, x1=ratio*i, length=0, angle=0, col=cole, lwd=4)}
for(i in 2:14){ points(ratio*i, att[i], pch=1, cex=4*prop[i], col="navy") }

text(x=ratio*2.5,y=0.135, labels="New Mexico as Control (2014)", font=2, cex=1.5)
text(x=ratio*0.5,y=att[1]-0.01, labels="All Voter", font=2)
text(x=ratio*2, y=att[2]+0.01, labels="Frequent Voter", font=2)
text(x=ratio*3, y=att[3]-0.01, labels="Infrequent Voter", font=2)
text(x=ratio*4, y=att[4]-0.01, labels="U40", font=2)
text(x=ratio*5, y=att[5]-0.01, labels="41-65", font=2)
text(x=ratio*6, y=att[6]-0.01, labels="O65", font=2)
text(x=ratio*7, y=att[7]+0.01, labels="White", font=2)
text(x=ratio*8, y=att[8]+0.01, labels="Black", font=2)
text(x=ratio*9, y=att[9]+0.01, labels="Hispanic", font=2)
text(x=ratio*10, y=att[10]+0.01, labels="Asian", font=2)
text(x=ratio*11, y=att[11]-0.01, labels="Female", font=2)
text(x=ratio*12, y=att[12]-0.01, labels="Male", font=2)
text(x=ratio*13, y=att[13]+0.01, labels="Dem", font=2)
text(x=ratio*14-1, y=att[14]+0.01, labels="Non-Dem", font=2)

dev.off()
}





################################################################################################
# END OF THIS R SOURCE CODE
################################################################################################
  

