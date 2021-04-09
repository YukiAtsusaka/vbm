################################################################################################
# 16_TrendsPlot.R
# Aim: to visualize the trends
################################################################################################

################################################################################################

library(tidyverse)
setwd("data")
####################################################
# (1) RAW DATA
####################################################
nc14 <- read_csv("Stack_Colorado_NC_2012_2014.csv")
nc16 <- read_csv("Stack_Colorado_NC_2012_2016.csv")
nm14 <- read_csv("Stack_Colorado_NM_2012_2014.csv")
nm16 <- read_csv("Stack_Colorado_NM_2012_2016.csv")


nc14_tr <- nc14 %>%
           group_by(Time, voted2010, Place) %>% 
           summarise(Turnout = mean(Vote))
nc14_tr

nc16_tr <- nc16 %>% group_by(Time, voted2010, Place) %>% 
           summarise(Turnout = mean(Vote))
nc16_tr

nm14_tr <- nm14 %>% group_by(Time, voted2010, Place) %>% 
           summarise(Turnout = mean(Vote))
nm14_tr

nm16_tr <- nm16 %>% group_by(Time, voted2010, Place) %>% 
           summarise(Turnout = mean(Vote))
nm16_tr

rm(nc14, nc16, nm14, nm16); gc(); gc()

####################################################
# (2) MATCHED DATA
####################################################

high <- read_csv(here::here("data/NC14Sample/2_NC2014_SampleFreq.csv"))
low <- read_csv(here::here("data/NC14Sample/3_NC2014_SampleInfreq.csv"))
match_nc14 <- rbind(high, low) %>%
              group_by(Time, voted2010, Place) %>% 
              summarise(Turnout = mean(Vote))

high <- read_csv(here::here("data/NC16Sample/2_NC2016_SampleFreq.csv"))
low <- read_csv(here::here("data/NC16Sample/3_NC2016_SampleInfreq.csv"))
match_nc16 <- rbind(high, low) %>%
              group_by(Time, voted2010, Place) %>% 
              summarise(Turnout = mean(Vote))

high <- read_csv(here::here("data/NM14Sample/2_NM2014_SampleFreq.csv"))
low <- read_csv(here::here("data/NM14Sample/3_NM2014_SampleInfreq.csv"))
match_nm14 <- rbind(high, low) %>%
              group_by(Time, voted2010, Place) %>% 
              summarise(Turnout = mean(Vote))


high <- read_csv(here::here("data/NM16Sample/2_NM2016_SampleFreq.csv"))
low <- read_csv(here::here("data/NM16Sample/3_NM2016_SampleInfreq.csv"))
match_nm16 <- rbind(high, low) %>%
              group_by(Time, voted2010, Place) %>% 
              summarise(Turnout = mean(Vote))



dt_list <- list(nc14_tr, nc16_tr, nm14_tr, nm16_tr,
                match_nc14, match_nc16, match_nm14, match_nm16)
vec_title <- c("CO v. NC, 2014", "CO v. NC, 2016", "CO v. NM, 2014", "CO v. NM, 2016")
vec_title <- rep(vec_title,2)

####################################################
# (3) PLOT THE SAVE THE GRAPH
####################################################

{pdf(here::here("figures/TrendsPlot.pdf"), width=7, height=3.5)
par(mfrow=c(2,4), mar=c(2,2,2,1), oma=c(0,2,1,0)+0.01, mgp=c(3,0.6,0))

for(i in 1:8){
dt <- dt_list[[i]]



plot(dt$Time, dt$Turnout, type="n", xlab="", ylab="", xaxt="n", yaxt="n",ylim=c(0.1,1.1))
axis(2, at=seq(from=0.2,to=1, by=0.2), las=1)
axis(1, at=unique(dt$Time), labels=c("Before", "After"), las=1, cex.lab=1.2)

# HIGH-PROPENSITY VOTERS
lines(dt$Time[dt$voted2010==1 & dt$Place==1],
      dt$Turnout[dt$voted2010==1 & dt$Place==1], type="b", pch=19, col="firebrick4")
lines(dt$Time[dt$voted2010==1 & dt$Place==0],
      dt$Turnout[dt$voted2010==1 & dt$Place==0], type="b", pch=17)

# LOW-PROPENSITY VOTERS
lines(dt$Time[dt$voted2010==0 & dt$Place==1],
      dt$Turnout[dt$voted2010==0 & dt$Place==1], type="b", pch=19, lty=2, col="firebrick4")
lines(dt$Time[dt$voted2010==0 & dt$Place==0],
      dt$Turnout[dt$voted2010==0 & dt$Place==0], type="b", pch=17, lty=2)
title(vec_title[i])

# TEXT ANNOTATION
if(i==1){
points(0.1, 0.7, pch=19, cex=0.8, col="firebrick4")
points(0.1, 0.62, pch=17, cex=0.8)
text(0.3, 0.7, labels="Treated", cex=0.8, col="firebrick4")
text(0.3, 0.62, labels="Control", cex=0.8)
text(0.7, 1, labels="High-propensity \nvoters", font=2, col="dimgray", cex=0.8)
text(0.3, 0.23, labels="Low-propensity  \nvoters", font=2, col="dimgray", cex=0.8)
mtext("Raw Data", side=2, line=2)
}else if(i==5){
mtext("Matched Sample", side=2, line=2)   
} # END OF IF


} # END OF LOOP

dev.off()
}




################################################################################################
# END OF THIS R SOURCE CODE
################################################################################################
