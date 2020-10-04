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

pdf("CATTPlot_1416.pdf", width=6, height=5)
par(mar=c(2,2,1,0.5), oma = c(0,3,2,0) + 0.1, mfrow=c(2,2))

cs=1.5
ls=2
cole <- "black"
cole2 <- "firebrick3"

################################################################################################
# NORTH CAROLINA CONTROL (LOGISTIC REGRESSION)
################################################################################################

catt <- c(0.070, 0.101, 0.004, 0.081) # 14-Freq, 14-Infreq, 16-Freq, 16-Infreq
se  <- c(0.002, 0.003, 0.002, 0.003) # Freq, Infreq

y <- seq(from=0, to=0.13, length=101)
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

legend(-0.02,0.04, pch = c(6, 17), cex = 1, bty = "n", legend = c("Infrequent voters", "Frequent voters"))

mtext("North Carolina voters as control units", side=3, line=0.5, cex=0.9)
mtext("Estimated Effect of VBM", side=2, line=2.1, cex=0.8)



################################################################################################
# New MEXICO CONTROL  (LOGISTIC REGRESSION)
################################################################################################

catt <- c(0.025, 0.127, 0.043, 0.075) # 14-Infreq, 14-freq, 16-Infreq, 16-freq
se  <- c(0.002, 0.002, 0.002, 0.002) # Infreq, Freq

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

legend(-0.02,0.04, pch = c(6, 17), cex = 1, bty = "n", legend = c("Infrequent voters", "Frequent voters"))

mtext("New Mexico voters as control units", side=3, line=0.5, cex=0.9)


################################################################################################
# NORTH CAROLINA CONTROL (LOGISTIC REGRESSION)
################################################################################################

catt <- c(0.095,0.079,0.124, 0.037, 0.046, 0.105) # 14-Y, 14-M, 14-O, 16-Y, 16-M, 16-O
se   <- c(0.004,0.003,0.003, 0.004, 0.003, 0.003)  # 14-Y, 14-M, 14-O, 16-Y, 16-M, 16-O

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
       legend = c("Under 35", "35-65", "Over 65"))

mtext("Estimated Effect of VBM", side=2, line=2.1, cex=0.8)

################################################################################################
# NEW MEXICO (LOGISTIC REGRESSION)
################################################################################################

catt <- c(0.107,0.094,0.102, 0.024, 0.060, 0.069) # 14-Y, 14-M, 14-O, 16-Y, 16-M, 16-O
se   <- c(0.003,0.003,0.005, 0.003, 0.003, 0.002)  # 14-Y, 14-M, 14-O, 16-Y, 16-M, 16-O


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

legend(-0.02,0.05, pch = c(6, 16, 17), cex = 1, bty = "n", legend = c("Under 35", "35-65", "Over 65"))






dev.off()


################################################################################################
# END OF THIS R SOURCE CODE
################################################################################################
  

