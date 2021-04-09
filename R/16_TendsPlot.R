################################################################################################
# 16_TrendsPlot.R
# Aim: to visualize the trends
################################################################################################

################################################################################################

library(tidyverse)
setwd("data")
nc14 <- read_csv("Stack_Colorado_NC_2012_2014.csv")
nc16 <- read_csv("Stack_Colorado_NC_2012_2016.csv")
nm14 <- read_csv("Stack_Colorado_NM_2012_2014.csv")
nm16 <- read_csv("Stack_Colorado_NM_2012_2016.csv")


nc14_tr <- nc14 %>%
           group_by(Year, voted2010, State) %>% 
           summarise(Turnout = mean(Vote))
nc14_tr

nc16_tr <- nc16 %>% group_by(Year, voted2010, State) %>% 
           summarise(Turnout = mean(Vote))
nc16_tr

nm14_tr <- nm14 %>% group_by(Year, voted2010, State) %>% 
           summarise(Turnout = mean(Vote))
nm14_tr

nm16_tr <- nm16 %>% group_by(Year, voted2010, State) %>% 
           summarise(Turnout = mean(Vote))
nm16_tr




{pdf(here::here("figures/TrendsPlot.pdf"), width=7, height=2)
par(mfrow=c(1,4), mar=c(2,2,2,1), oma=c(0,0,1,0)+0.01, mgp=c(3,0.6,0))

plot(nc14_tr$Year, nc14_tr$Turnout, type="n", xlab="", ylab="", xaxt="n", yaxt="n",ylim=c(0.1,1.1))
axis(2, at=seq(from=0.2,to=1, by=0.2), las=1)
axis(1, at=unique(nc14_tr$Year), labels=c("Before", "After"), las=1, cex.lab=1.2)
lines(nc14_tr$Year[nc14_tr$voted2010==0 & nc14_tr$State=="Colorado"], 
      nc14_tr$Turnout[nc14_tr$voted2010==0 & nc14_tr$State=="Colorado"], type="b", pch=19, lty=2, col="firebrick4")
lines(nc14_tr$Year[nc14_tr$voted2010==0 & nc14_tr$State=="North Carolina"], 
      nc14_tr$Turnout[nc14_tr$voted2010==0 & nc14_tr$State=="North Carolina"], type="b", pch=17, lty=2)
lines(nc14_tr$Year[nc14_tr$voted2010==1 & nc14_tr$State=="Colorado"], 
      nc14_tr$Turnout[nc14_tr$voted2010==1 & nc14_tr$State=="Colorado"], type="b", pch=19, col="firebrick4")
lines(nc14_tr$Year[nc14_tr$voted2010==1 & nc14_tr$State=="North Carolina"], 
      nc14_tr$Turnout[nc14_tr$voted2010==1 & nc14_tr$State=="North Carolina"], type="b", pch=17)
title("CO v. NC, 2014")
points(2012.1, 0.7, pch=19, cex=0.8, col="firebrick4")
points(2012.1, 0.62, pch=17, cex=0.8)
text(2012.5, 0.7, labels="Treated", cex=0.8, col="firebrick4")
text(2012.5, 0.62, labels="Control", cex=0.8)
text(2013.5, 1, labels="High-propensity \nvoters", font=2, col="dimgray", cex=0.8)
text(2012.6, 0.23, labels="Low-propensity  \nvoters", font=2, col="dimgray", cex=0.8)

plot(nc16_tr$Year, nc16_tr$Turnout, type="n", xlab="", ylab="", xaxt="n", yaxt="n", ylim=c(0.1,1.1))
axis(2, at=seq(from=0.2,to=1, by=0.2), las=1)
axis(1, at=unique(nc16_tr$Year), labels=c("Before", "After"), las=1, cex.lab=1.2)
lines(nc16_tr$Year[nc16_tr$voted2010==0 & nc16_tr$State=="Colorado"], 
      nc16_tr$Turnout[nc16_tr$voted2010==0 & nc16_tr$State=="Colorado"], type="b", pch=19, lty=2, col="firebrick4")
lines(nc16_tr$Year[nc16_tr$voted2010==0 & nc16_tr$State=="North Carolina"], 
      nc16_tr$Turnout[nc16_tr$voted2010==0 & nc16_tr$State=="North Carolina"], type="b", pch=17, lty=2)
lines(nc16_tr$Year[nc16_tr$voted2010==1 & nc16_tr$State=="Colorado"], 
      nc16_tr$Turnout[nc16_tr$voted2010==1 & nc16_tr$State=="Colorado"], type="b", pch=19, col="firebrick4")
lines(nc16_tr$Year[nc16_tr$voted2010==1 & nc16_tr$State=="North Carolina"], 
      nc16_tr$Turnout[nc16_tr$voted2010==1 & nc16_tr$State=="North Carolina"], type="b", pch=17)
title("CO v. NC, 2016")


plot(nm14_tr$Year, nm14_tr$Turnout, type="n", xlab="", ylab="", xaxt="n", yaxt="n", ylim=c(0.1,1.1))
axis(2, at=seq(from=0.2,to=1, by=0.2), las=1)
axis(1, at=unique(nm14_tr$Year), labels=c("Before", "After"), las=1, cex.lab=1.2)
lines(nm14_tr$Year[nm14_tr$voted2010==0 & nm14_tr$State=="Colorado"], 
      nm14_tr$Turnout[nm14_tr$voted2010==0 & nm14_tr$State=="Colorado"], type="b", pch=19, lty=2, col="firebrick4")
lines(nm14_tr$Year[nm14_tr$voted2010==0 & nm14_tr$State=="New Mexico"], 
      nm14_tr$Turnout[nm14_tr$voted2010==0 & nm14_tr$State=="New Mexico"], type="b", pch=17, lty=2)
lines(nm14_tr$Year[nm14_tr$voted2010==1 & nm14_tr$State=="Colorado"], 
      nm14_tr$Turnout[nm14_tr$voted2010==1 & nm14_tr$State=="Colorado"], type="b", pch=19, col="firebrick4")
lines(nm14_tr$Year[nm14_tr$voted2010==1 & nm14_tr$State=="New Mexico"], 
      nm14_tr$Turnout[nm14_tr$voted2010==1 & nm14_tr$State=="New Mexico"], type="b", pch=17)
title("CO v. NM, 2014")

plot(nm16_tr$Year, nm16_tr$Turnout, type="n", xlab="", ylab="", xaxt="n", yaxt="n", ylim=c(0.1,1.1))
axis(2, at=seq(from=0.2,to=1, by=0.2), las=1)
axis(1, at=unique(nm16_tr$Year), labels=c("Before", "After"), las=1, cex.lab=1.2)
lines(nm16_tr$Year[nm16_tr$voted2010==0 & nm16_tr$State=="Colorado"], 
      nm16_tr$Turnout[nm16_tr$voted2010==0 & nm16_tr$State=="Colorado"], type="b", pch=19, lty=2, col="firebrick4")
lines(nm16_tr$Year[nm16_tr$voted2010==0 & nm16_tr$State=="New Mexico"], 
      nm16_tr$Turnout[nm16_tr$voted2010==0 & nm16_tr$State=="New Mexico"], type="b", pch=17, lty=2)
lines(nm16_tr$Year[nm16_tr$voted2010==1 & nm16_tr$State=="Colorado"], 
      nm16_tr$Turnout[nm16_tr$voted2010==1 & nm16_tr$State=="Colorado"], type="b", pch=19, col="firebrick4")
lines(nm16_tr$Year[nm16_tr$voted2010==1 & nm16_tr$State=="New Mexico"], 
      nm16_tr$Turnout[nm16_tr$voted2010==1 & nm16_tr$State=="New Mexico"], type="b", pch=17)
title("CO v. NM, 2016")

dev.off()
}




################################################################################################
# END OF THIS R SOURCE CODE
################################################################################################
