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

pdf("CATTPlot_1416.pdf", width=8, height=4)
par(mar=c(3,3,3,1), mfrow=c(1,2))

cs=1.5
ls=2

################################################################################################
# NORTH CAROLINA CONTROL
################################################################################################

eff.2014 <- c(0.101, 0.070)     # Freq, Infreq
eff.2014.se  <- c(0.003, 0.002) # Freq, Infreq
eff.2016 <- c(0.081, 0.004)     # Freq, Infreq
eff.2016.se  <- c(0.003, 0.002) # Freq, Infreq

y <- seq(from=0, to=0.13, length=101)
x <- seq(from=0, to=1, length=101)
cole <- "black"

plot(y ~ x, type="n", xlab="", ylab="", xaxt="n")
abline(h=0, lty=2)

arrows(y0=eff.2014[1]-1.96*eff.2014.se[1], y1=eff.2014[1]+1.96*eff.2014.se[1],
       x0=0.2, x1=0.2, length=0, angle=0, col=cole, lwd=4)
points(0.2, eff.2014[1], pch=16, cex=1.5)

arrows(y0=eff.2014[2]-1.96*eff.2014.se[2], y1=eff.2014[2]+1.96*eff.2014.se[2],
       x0=0.8, x1=0.8, length=0, angle=0, col=cole, lwd=4)
points(0.8, eff.2014[2], pch=16, cex=1.5)

arrows(y0=eff.2016[1]-1.96*eff.2016.se[1], y1=eff.2016[1]+1.96*eff.2016.se[1],
       x0=0.3, x1=0.3, length=0, angle=0, col=cole, lwd=4)
points(0.3, eff.2016[1], pch=6, cex=1.5)

arrows(y0=eff.2016[2]-1.96*eff.2016.se[2], y1=eff.2016[2]+1.96*eff.2016.se[2],
       x0=0.9, x1=0.9, length=0, angle=0, col=cole, lwd=4)
points(0.9, eff.2016[2], pch=6, cex=1.5)

axis(1, at = seq(0.25, 1, by = 1), las=1, labels="Frequent \n voters",cex.axis=1, mgp=c(0,1.4,0))
axis(1, at = seq(0.85, 1, by = 1), las=1, labels="Infrequent \n voters",cex.axis=1,  mgp=c(0,1.4,0))
title("North Carolina as Control", line=0.5)

legend(0,0.04, pch = c(16, 6), cex = 1.2, bty = "n",
       legend = c("2014 election", "2016 election"))

################################################################################################
# New MEXICO CONTROL
################################################################################################

eff.2014 <- c(0.124, 0.116)     # Freq, Infreq
eff.2014.se  <- c(0.001, 0.002) # Freq, Infreq
eff.2016 <- c(0.079, 0.051)     # Freq, Infreq
eff.2016.se  <- c(0.002, 0.002) # Freq, Infreq


plot(y ~ x, type="n", xlab="", ylab="", xaxt="n")
abline(h=0, lty=2)

arrows(y0=eff.2014[1]-1.96*eff.2014.se[1], y1=eff.2014[1]+1.96*eff.2014.se[1],
       x0=0.2, x1=0.2, length=0, angle=0, col=cole, lwd=4)
points(0.2, eff.2014[1], pch=16, cex=1.5)

arrows(y0=eff.2014[2]-1.96*eff.2014.se[2], y1=eff.2014[2]+1.96*eff.2014.se[2],
       x0=0.8, x1=0.8, length=0, angle=0, col=cole, lwd=4)
points(0.8, eff.2014[2], pch=16, cex=1.5)

arrows(y0=eff.2016[1]-1.96*eff.2016.se[1], y1=eff.2016[1]+1.96*eff.2016.se[1],
       x0=0.3, x1=0.3, length=0, angle=0, col=cole, lwd=4)
points(0.3, eff.2016[1], pch=6, cex=1.5)

arrows(y0=eff.2016[2]-1.96*eff.2016.se[2], y1=eff.2016[2]+1.96*eff.2016.se[2],
       x0=0.9, x1=0.9, length=0, angle=0, col=cole, lwd=4)
points(0.9, eff.2016[2], pch=6, cex=1.5)

axis(1, at = seq(0.25, 1, by = 1), las=1, labels="Frequent \n voters",cex.axis=1, mgp=c(0,1.4,0))
axis(1, at = seq(0.85, 1, by = 1), las=1, labels="Infrequent \n voters",cex.axis=1,  mgp=c(0,1.4,0))
title("New Mexico as Control", line=0.5)




dev.off()

################################################################################################
# END OF THIS R SOURCE CODE
################################################################################################
  

