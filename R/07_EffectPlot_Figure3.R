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



{pdf("CATTPlot_Figure3.pdf", width=6, height=3)
par(mar=c(2,2,1,0.5), oma = c(0,3,2,0) + 0.1, mfrow=c(1,2))

cs=1.5
ls=2
col <- "black"
col2 <- "gray60"
y <- seq(from=0, to=0.14, length=101)
x <- seq(from=0, to=1, length=101)


################################################################################################
# NORTH CAROLINA CONTROL
################################################################################################

catt <- c(nc14[1:3], nc16[1:3])
se <- c(nc14se[1:3], nc16se[1:3])

plot(y ~ x, type="n", xlab="", ylab="", xaxt="n")
abline(h=0, lty=2)

arrows(y0=catt[1]-1.96*se[1], y1=catt[1]+1.96*se[1], x0=0.15, x1=0.15, length=0, angle=0, col=col2, lwd=ls)
points(0.15, catt[1], pch=16, cex=cs, col=col2)
arrows(y0=catt[2]-1.96*se[2], y1=catt[2]+1.96*se[2], x0=0.25, x1=0.25, length=0, angle=0, col=col, lwd=ls)
points(0.25, catt[2], pch=6, cex=cs)
arrows(y0=catt[3]-1.96*se[3], y1=catt[3]+1.96*se[3], x0=0.35, x1=0.35, length=0, angle=0, col=col, lwd=ls)
points(0.35, catt[3], pch=17, cex=cs)

arrows(y0=catt[4]-1.96*se[4], y1=catt[4]+1.96*se[4], x0=0.65, x1=0.65, length=0, angle=0, col=col2, lwd=ls)
points(0.65, catt[4], pch=16, cex=cs, col=col2)
arrows(y0=catt[5]-1.96*se[5], y1=catt[5]+1.96*se[5], x0=0.75, x1=0.75, length=0, angle=0, col=col, lwd=ls)
points(0.75, catt[5], pch=6, cex=cs)
arrows(y0=catt[6]-1.96*se[6], y1=catt[6]+1.96*se[6], x0=0.85, x1=0.85, length=0, angle=0, col=col, lwd=ls)
points(0.85, catt[6], pch=17, cex=cs)

axis(1, at = seq(0.25, 1, by = 1), las=1, labels="2014",cex.axis=1, mgp=c(0,0.8,0))
axis(1, at = seq(0.75, 1, by = 1), las=1, labels="2016" ,cex.axis=1,  mgp=c(0,0.8,0))

legend(-0.02,0.05, pch = c(16, 6, 17), cex = 0.8, bty = "n", col=c(col2, col,col),
       legend = c("All voters", "Frequent voters", "Infrequent voters"))

mtext("Estimated Effect of VBM", side=2, line=2.1, cex=0.8)
mtext("North Carolina voters as control units", side=3, line=0.5, cex=0.9)

################################################################################################
# NEW MEXICO (
################################################################################################

catt <- c(nm14[1:3], nm16[1:3])
se <- c(nm14se[1:3], nm16se[1:3])

plot(y ~ x, type="n", xlab="", ylab="", xaxt="n")
abline(h=0, lty=2)

arrows(y0=catt[1]-1.96*se[1], y1=catt[1]+1.96*se[1], x0=0.15, x1=0.15, length=0, angle=0, col=col2, lwd=ls)
points(0.15, catt[1], pch=16, cex=cs, col=col2)
arrows(y0=catt[2]-1.96*se[2], y1=catt[2]+1.96*se[2], x0=0.25, x1=0.25, length=0, angle=0, col=col, lwd=ls)
points(0.25, catt[2], pch=6, cex=cs)
arrows(y0=catt[3]-1.96*se[3], y1=catt[3]+1.96*se[3], x0=0.35, x1=0.35, length=0, angle=0, col=col, lwd=ls)
points(0.35, catt[3], pch=17, cex=cs)

arrows(y0=catt[4]-1.96*se[4], y1=catt[4]+1.96*se[4], x0=0.65, x1=0.65, length=0, angle=0, col=col2, lwd=ls)
points(0.65, catt[4], pch=16, cex=cs, col=col2)
arrows(y0=catt[5]-1.96*se[5], y1=catt[5]+1.96*se[5], x0=0.75, x1=0.75, length=0, angle=0, col=col, lwd=ls)
points(0.75, catt[5], pch=6, cex=cs)
arrows(y0=catt[6]-1.96*se[6], y1=catt[6]+1.96*se[6], x0=0.85, x1=0.85, length=0, angle=0, col=col, lwd=ls)
points(0.85, catt[6], pch=17, cex=cs)

axis(1, at = seq(0.25, 1, by = 1), las=1, labels="2014",cex.axis=1, mgp=c(0,0.8,0))
axis(1, at = seq(0.75, 1, by = 1), las=1, labels="2016" ,cex.axis=1,  mgp=c(0,0.8,0))

legend(-0.02,0.05, pch = c(16, 6, 17), cex = 0.8, bty = "n", col=c(col2, col,col),
       legend = c("All voters", "Frequent voters", "Infrequent voters"))
mtext("New Mexico voters as control units", side=3, line=0.5, cex=0.9)


dev.off()}


################################################################################################
# END OF THIS R SOURCE CODE
################################################################################################
  

