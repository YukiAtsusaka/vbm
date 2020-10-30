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
rm(list=ls())
setwd("R")
est <- read_csv("ATT_NC14.csv")
nc14 <- est[,1] %>% pull(); nc14se <- est[,2] %>% pull()
est <- read_csv("ATT_NC16.csv")
nc16 <- est[,1] %>% pull(); nc16se <- est[,2] %>% pull()
est <- read_csv("ATT_NM14.csv")
nm14 <- est[,1] %>% pull(); nm14se <- est[,2] %>% pull()
est <- read_csv("ATT_NM16.csv")
nm16 <- est[,1] %>% pull(); nm16se <- est[,2] %>% pull()



{pdf("CATTPlot_Figure3_old.pdf", width=6, height=5)
par(mar=c(2,2,1,0.5), oma = c(0,3,2,0) + 0.1, mfrow=c(2,2))

cs=1.5
ls=2
cole <- "black"
cole2 <- "black"

################################################################################################
# NORTH CAROLINA CONTROL (FREQUENCY OF VOTING)
################################################################################################

catt <- c(nc14[2:3], nc16[2:3])
se <- c(nc14se[2:3], nc16se[2:3])

y <- seq(from=0, to=0.14, length=101)
x <- seq(from=0, to=1, length=101)

plot(y ~ x, type="n", xlab="", ylab="", xaxt="n")
abline(h=0, lty=2)

arrows(y0=catt[1]-1.96*se[1], y1=catt[1]+1.96*se[1], x0=0.2, x1=0.2, length=0, angle=0, col=cole, lwd=ls)
points(0.2, catt[1], pch=6, cex=cs)

arrows(y0=catt[2]-1.96*se[2], y1=catt[2]+1.96*se[2], x0=0.3, x1=0.3, length=0, angle=0, col=cole, lwd=ls)
points(0.3, catt[2], pch=17, cex=cs)


arrows(y0=catt[3]-1.96*se[3], y1=catt[3]+1.96*se[3], x0=0.7, x1=0.7, length=0, angle=0, col=cole2, lwd=ls)
points(0.7, catt[3], pch=6, cex=cs, col=cole2)

arrows(y0=catt[4]-1.96*se[4], y1=catt[4]+1.96*se[4], x0=0.8, x1=0.8, length=0, angle=0, lwd=ls)
points(0.8, catt[4], pch=17, cex=cs)

axis(1, at = seq(0.25, 1, by = 1), las=1, labels="2014",cex.axis=1, mgp=c(0,0.8,0))
axis(1, at = seq(0.75, 1, by = 1), las=1, labels="2016" ,cex.axis=1,  mgp=c(0,0.8,0))

legend(-0.02,0.04, pch = c(6, 17), cex = 1, bty = "n", legend = c("Frequent voters", "Infrequent voters"))

mtext("North Carolina voters as control units", side=3, line=0.5, cex=0.9)
mtext("Estimated Effect of VBM", side=2, line=2.1, cex=0.8)



################################################################################################
# New MEXICO CONTROL (FREQUENCY OF VOTING)
################################################################################################

catt <- c(nm14[2:3], nm16[2:3])
se <- c(nm14se[2:3], nm16se[2:3])


plot(y ~ x, type="n", xlab="", ylab="", xaxt="n")
abline(h=0, lty=2)

arrows(y0=catt[1]-1.96*se[1], y1=catt[1]+1.96*se[1], x0=0.2, x1=0.2, length=0, angle=0, col=cole, lwd=ls)
points(0.2, catt[1], pch=6, cex=cs)

arrows(y0=catt[2]-1.96*se[2], y1=catt[2]+1.96*se[2], x0=0.3, x1=0.3, length=0, angle=0, col=cole, lwd=ls)
points(0.3, catt[2], pch=17, cex=cs)


arrows(y0=catt[3]-1.96*se[3], y1=catt[3]+1.96*se[3], x0=0.7, x1=0.7, length=0, angle=0, col=cole2, lwd=ls)
points(0.7, catt[3], pch=6, cex=cs, col=cole2)

arrows(y0=catt[4]-1.96*se[4], y1=catt[4]+1.96*se[4], x0=0.8, x1=0.8, length=0, angle=0, col=cole, lwd=ls)
points(0.8, catt[4], pch=17, cex=cs)

axis(1, at = seq(0.25, 1, by = 1), las=1, labels="2014",cex.axis=1, mgp=c(0,0.8,0))
axis(1, at = seq(0.75, 1, by = 1), las=1, labels="2016" ,cex.axis=1,  mgp=c(0,0.8,0))

legend(-0.02,0.04, pch = c(6, 17), cex = 1, bty = "n", legend = c("Frequent voters", "Infrequent voters"))

mtext("New Mexico voters as control units", side=3, line=0.5, cex=0.9)


################################################################################################
# NORTH CAROLINA CONTROL (AGE GROUP)
################################################################################################

catt <- c(nc14[4:6], nc16[4:6])
se <- c(nc14se[4:6], nc16se[4:6])

plot(y ~ x, type="n", xlab="", ylab="", xaxt="n")
abline(h=0, lty=2)

arrows(y0=catt[1]-1.96*se[1], y1=catt[1]+1.96*se[1], x0=0.2, x1=0.2, length=0, angle=0, col=cole, lwd=ls)
points(0.2, catt[1], pch=6, cex=cs)
arrows(y0=catt[2]-1.96*se[2], y1=catt[2]+1.96*se[2], x0=0.25, x1=0.25, length=0, angle=0, col=cole, lwd=ls)
points(0.25, catt[2], pch=16, cex=cs)
arrows(y0=catt[3]-1.96*se[3], y1=catt[3]+1.96*se[3], x0=0.3, x1=0.3, length=0, angle=0, col=cole, lwd=ls)
points(0.3, catt[3], pch=17, cex=cs)

arrows(y0=catt[4]-1.96*se[4], y1=catt[4]+1.96*se[4], x0=0.7, x1=0.7, length=0, angle=0, col=cole2, lwd=ls)
points(0.7, catt[4], pch=6, cex=cs, col=cole2)
arrows(y0=catt[5]-1.96*se[5], y1=catt[5]+1.96*se[5], x0=0.75, x1=0.75, length=0, angle=0, col=cole, lwd=ls)
points(0.75, catt[5], pch=16, cex=cs)
arrows(y0=catt[6]-1.96*se[6], y1=catt[6]+1.96*se[6], x0=0.8, x1=0.8, length=0, angle=0, col=cole, lwd=ls)
points(0.8, catt[6], pch=17, cex=cs)

axis(1, at = seq(0.25, 1, by = 1), las=1, labels="2014",cex.axis=1, mgp=c(0,0.8,0))
axis(1, at = seq(0.75, 1, by = 1), las=1, labels="2016" ,cex.axis=1,  mgp=c(0,0.8,0))

legend(-0.02,0.05, pch = c(6, 16, 17), cex = 1, bty = "n",
       legend = c("Under 40", "41-65", "Over 65"))

mtext("Estimated Effect of VBM", side=2, line=2.1, cex=0.8)

################################################################################################
# NEW MEXICO (AGE GROUP)
################################################################################################

catt <- c(nm14[4:6], nm16[4:6])
se <- c(nm14se[4:6], nm16se[4:6])

plot(y ~ x, type="n", xlab="", ylab="", xaxt="n")
abline(h=0, lty=2)

arrows(y0=catt[1]-1.96*se[1], y1=catt[1]+1.96*se[1], x0=0.2, x1=0.2, length=0, angle=0, col=cole, lwd=ls)
points(0.2, catt[1], pch=6, cex=cs)
arrows(y0=catt[2]-1.96*se[2], y1=catt[2]+1.96*se[2], x0=0.25, x1=0.25, length=0, angle=0, col=cole, lwd=ls)
points(0.25, catt[2], pch=16, cex=cs)
arrows(y0=catt[3]-1.96*se[3], y1=catt[3]+1.96*se[3], x0=0.3, x1=0.3, length=0, angle=0, col=cole, lwd=ls)
points(0.3, catt[3], pch=17, cex=cs)

arrows(y0=catt[4]-1.96*se[4], y1=catt[4]+1.96*se[4], x0=0.7, x1=0.7, length=0, angle=0, col=cole2, lwd=ls)
points(0.7, catt[4], pch=6, cex=cs, col=cole2)
arrows(y0=catt[5]-1.96*se[5], y1=catt[5]+1.96*se[5], x0=0.75, x1=0.75, length=0, angle=0, col=cole, lwd=ls)
points(0.75, catt[5], pch=16, cex=cs)
arrows(y0=catt[6]-1.96*se[6], y1=catt[6]+1.96*se[6], x0=0.8, x1=0.8, length=0, angle=0, col=cole, lwd=ls)
points(0.8, catt[6], pch=17, cex=cs)

axis(1, at = seq(0.25, 1, by = 1), las=1, labels="2014",cex.axis=1, mgp=c(0,0.8,0))
axis(1, at = seq(0.75, 1, by = 1), las=1, labels="2016" ,cex.axis=1,  mgp=c(0,0.8,0))

legend(-0.02,0.05, pch = c(6, 16, 17), cex = 1, bty = "n", legend = c("Under 40", "41-65", "Over 65"))

dev.off()}


################################################################################################
# END OF THIS R SOURCE CODE
################################################################################################
  

