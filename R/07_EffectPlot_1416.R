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

pdf("CATTPlot_1416.pdf", width=6, height=5.5)
par(mar=c(1.5,3,1,1), mfrow=c(3,2))

cs=1.5
ls=2

################################################################################################
# NORTH CAROLINA CONTROL (LOGISTIC REGRESSION)
################################################################################################

catt <- c(0.101, 0.070, 0.081, 0.004) # 14-Freq, 14-Infreq, 16-Freq, 16-Infreq
se  <- c(0.003, 0.002, 0.003, 0.002) # Freq, Infreq

y <- seq(from=-0.04, to=0.17, length=101)
x <- seq(from=0, to=1, length=101)
cole <- "black"

plot(y ~ x, type="n", xlab="", ylab="", xaxt="n")
abline(h=0, lty=2)

arrows(y0=catt[1]-1.96*se[1], y1=catt[1]+1.96*se[1], x0=0.2, x1=0.2, length=0, angle=0, col=cole, lwd=ls)
points(0.2, catt[1], pch=16, cex=cs)

arrows(y0=catt[2]-1.96*se[2], y1=catt[2]+1.96*se[2], x0=0.3, x1=0.3, length=0, angle=0, col=cole, lwd=ls)
points(0.3, catt[2], pch=6, cex=cs)


arrows(y0=catt[3]-1.96*se[3], y1=catt[3]+1.96*se[3], x0=0.8, x1=0.8, length=0, angle=0, col=cole, lwd=ls)
points(0.8, catt[3], pch=16, cex=cs)

arrows(y0=catt[4]-1.96*se[4], y1=catt[4]+1.96*se[4], x0=0.9, x1=0.9, length=0, angle=0, col=cole, lwd=ls)
points(0.9, catt[4], pch=6, cex=cs)

axis(1, at = seq(0.25, 1, by = 1), las=1, labels="2014",cex.axis=1, mgp=c(0,1.4,0))
axis(1, at = seq(0.85, 1, by = 1), las=1, labels="2016" ,cex.axis=1,  mgp=c(0,1.4,0))
#title("North Carolina as Control", line=2, cex=2)

legend(0,0.04, pch = c(16, 6), cex = 1.2, bty = "n", legend = c("Frequent voters", "Infrequent voters"))

################################################################################################
# New MEXICO CONTROL  (LOGISTIC REGRESSION)
################################################################################################

catt <- c(0.124, 0.116, 0.079, 0.051) # 14-Freq, 14-Infreq, 16-Freq, 16-Infreq
se  <- c(0.001, 0.002, 0.002, 0.002) # Freq, Infreq

y <- seq(from=-0.04, to=0.17, length=101)
x <- seq(from=0, to=1, length=101)
cole <- "black"

plot(y ~ x, type="n", xlab="", ylab="", xaxt="n")
abline(h=0, lty=2)

arrows(y0=catt[1]-1.96*se[1], y1=catt[1]+1.96*se[1], x0=0.2, x1=0.2, length=0, angle=0, col=cole, lwd=ls)
points(0.2, catt[1], pch=16, cex=cs)

arrows(y0=catt[2]-1.96*se[2], y1=catt[2]+1.96*se[2], x0=0.3, x1=0.3, length=0, angle=0, col=cole, lwd=ls)
points(0.3, catt[2], pch=6, cex=cs)


arrows(y0=catt[3]-1.96*se[3], y1=catt[3]+1.96*se[3], x0=0.8, x1=0.8, length=0, angle=0, col=cole, lwd=ls)
points(0.8, catt[3], pch=16, cex=cs)

arrows(y0=catt[4]-1.96*se[4], y1=catt[4]+1.96*se[4], x0=0.9, x1=0.9, length=0, angle=0, col=cole, lwd=ls)
points(0.9, catt[4], pch=6, cex=cs)

axis(1, at = seq(0.25, 1, by = 1), las=1, labels="2014",cex.axis=1, mgp=c(0,1.4,0))
axis(1, at = seq(0.85, 1, by = 1), las=1, labels="2016" ,cex.axis=1,  mgp=c(0,1.4,0))
#title("New Mexico as Control", line=2, cex=2)



################################################################################################
# NORTH CAROLINA CONTROL (LOWER PARTIAL IDENTIFICATION)
################################################################################################

catt <- c(0.117, 0.165, 0.054, 0.035) # 14-Freq, 14-Infreq, 16-Freq, 16-Infreq
se   <- c(0.002, 0.002, 0.002, 0.002) # Freq, Infreq

y <- seq(from=-0.04, to=0.17, length=101)
x <- seq(from=0, to=1, length=101)
cole <- "black"

plot(y ~ x, type="n", xlab="", ylab="", xaxt="n")
abline(h=0, lty=2)

arrows(y0=catt[1]-1.96*se[1], y1=catt[1]+1.96*se[1], x0=0.2, x1=0.2, length=0, angle=0, col=cole, lwd=ls)
points(0.2, catt[1], pch=16, cex=cs)

arrows(y0=catt[2]-1.96*se[2], y1=catt[2]+1.96*se[2], x0=0.3, x1=0.3, length=0, angle=0, col=cole, lwd=ls)
points(0.3, catt[2], pch=6, cex=cs)


arrows(y0=catt[3]-1.96*se[3], y1=catt[3]+1.96*se[3], x0=0.8, x1=0.8, length=0, angle=0, col=cole, lwd=ls)
points(0.8, catt[3], pch=16, cex=cs)

arrows(y0=catt[4]-1.96*se[4], y1=catt[4]+1.96*se[4], x0=0.9, x1=0.9, length=0, angle=0, col=cole, lwd=ls)
points(0.9, catt[4], pch=6, cex=cs)

axis(1, at = seq(0.25, 1, by = 1), las=1, labels="2014",cex.axis=1, mgp=c(0,1.4,0))
axis(1, at = seq(0.85, 1, by = 1), las=1, labels="2016" ,cex.axis=1,  mgp=c(0,1.4,0))


################################################################################################
# NEW MEXIXO CONTROL (LOWER PARTIAL IDENTIFICATION)
################################################################################################

catt <- c(0.167, 0.146, 0.050, 0.063) # 14-Freq, 14-Infreq, 16-Freq, 16-Infreq
se   <- c(0.001, 0.002, 0.001, 0.002) # Freq, Infreq

y <- seq(from=-0.04, to=0.17, length=101)
x <- seq(from=0, to=1, length=101)
cole <- "black"

plot(y ~ x, type="n", xlab="", ylab="", xaxt="n")
abline(h=0, lty=2)

arrows(y0=catt[1]-1.96*se[1], y1=catt[1]+1.96*se[1], x0=0.2, x1=0.2, length=0, angle=0, col=cole, lwd=ls)
points(0.2, catt[1], pch=16, cex=cs)

arrows(y0=catt[2]-1.96*se[2], y1=catt[2]+1.96*se[2], x0=0.3, x1=0.3, length=0, angle=0, col=cole, lwd=ls)
points(0.3, catt[2], pch=6, cex=cs)


arrows(y0=catt[3]-1.96*se[3], y1=catt[3]+1.96*se[3], x0=0.8, x1=0.8, length=0, angle=0, col=cole, lwd=ls)
points(0.8, catt[3], pch=16, cex=cs)

arrows(y0=catt[4]-1.96*se[4], y1=catt[4]+1.96*se[4], x0=0.9, x1=0.9, length=0, angle=0, col=cole, lwd=ls)
points(0.9, catt[4], pch=6, cex=cs)

axis(1, at = seq(0.25, 1, by = 1), las=1, labels="2014",cex.axis=1, mgp=c(0,1.4,0))
axis(1, at = seq(0.85, 1, by = 1), las=1, labels="2016" ,cex.axis=1,  mgp=c(0,1.4,0))


################################################################################################
# NORTH CAROLINA CONTROL (UPPER PARTIAL IDENTIFICATION)
################################################################################################

catt <- c(0.108, 0.009, 0.081, -0.035) # 14-Freq, 14-Infreq, 16-Freq, 16-Infreq
se   <- c(0.003, 0.002, 0.003, 0.002) # Freq, Infreq

y <- seq(from=-0.04, to=0.17, length=101)
x <- seq(from=0, to=1, length=101)
cole <- "black"

plot(y ~ x, type="n", xlab="", ylab="", xaxt="n")
abline(h=0, lty=2)

arrows(y0=catt[1]-1.96*se[1], y1=catt[1]+1.96*se[1], x0=0.2, x1=0.2, length=0, angle=0, col=cole, lwd=ls)
points(0.2, catt[1], pch=16, cex=cs)

arrows(y0=catt[2]-1.96*se[2], y1=catt[2]+1.96*se[2], x0=0.3, x1=0.3, length=0, angle=0, col=cole, lwd=ls)
points(0.3, catt[2], pch=6, cex=cs)


arrows(y0=catt[3]-1.96*se[3], y1=catt[3]+1.96*se[3], x0=0.8, x1=0.8, length=0, angle=0, col=cole, lwd=ls)
points(0.8, catt[3], pch=16, cex=cs)

arrows(y0=catt[4]-1.96*se[4], y1=catt[4]+1.96*se[4], x0=0.9, x1=0.9, length=0, angle=0, col=cole, lwd=ls)
points(0.9, catt[4], pch=6, cex=cs)

axis(1, at = seq(0.25, 1, by = 1), las=1, labels="2014",cex.axis=1, mgp=c(0,1.4,0))
axis(1, at = seq(0.85, 1, by = 1), las=1, labels="2016" ,cex.axis=1,  mgp=c(0,1.4,0))


################################################################################################
# NEW MEXIXO CONTROL (UPPER PARTIAL IDENTIFICATION)
################################################################################################

catt <- c(0.123, 0.104, 0.083, -0.004) # 14-Freq, 14-Infreq, 16-Freq, 16-Infreq
se   <- c(0.002, 0.002, 0.002, 0.002) # Freq, Infreq

y <- seq(from=-0.04, to=0.17, length=101)
x <- seq(from=0, to=1, length=101)
cole <- "black"

plot(y ~ x, type="n", xlab="", ylab="", xaxt="n")
abline(h=0, lty=2)

arrows(y0=catt[1]-1.96*se[1], y1=catt[1]+1.96*se[1], x0=0.2, x1=0.2, length=0, angle=0, col=cole, lwd=ls)
points(0.2, catt[1], pch=16, cex=cs)

arrows(y0=catt[2]-1.96*se[2], y1=catt[2]+1.96*se[2], x0=0.3, x1=0.3, length=0, angle=0, col=cole, lwd=ls)
points(0.3, catt[2], pch=6, cex=cs)


arrows(y0=catt[3]-1.96*se[3], y1=catt[3]+1.96*se[3], x0=0.8, x1=0.8, length=0, angle=0, col=cole, lwd=ls)
points(0.8, catt[3], pch=16, cex=cs)

arrows(y0=catt[4]-1.96*se[4], y1=catt[4]+1.96*se[4], x0=0.9, x1=0.9, length=0, angle=0, col=cole, lwd=ls)
points(0.9, catt[4], pch=6, cex=cs)

axis(1, at = seq(0.25, 1, by = 1), las=1, labels="2014",cex.axis=1, mgp=c(0,1.4,0))
axis(1, at = seq(0.85, 1, by = 1), las=1, labels="2016" ,cex.axis=1,  mgp=c(0,1.4,0))


dev.off()

################################################################################################
# END OF THIS R SOURCE CODE
################################################################################################
  

