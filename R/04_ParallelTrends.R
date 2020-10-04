################################################################################################
# DID_BVMEffect.R
# Created by Yuki Atsusaka
# Since 8/27/2019
# Last updated 8/30/2019
# Aim: to use the difference-in-differences estimator for the ATE of the VBM policy in Colorado
################################################################################################

################################################################################################
# FIGURE IN SECTION 6
# (999) ROBUSTNESS CHECK WITH CCES
library(tidyverse)
dat <- haven::read_dta("cumulative_2006_2018.dta")

cces <- dat %>% filter(state==37 | state==8 | state==35) %>%
         filter(year %in% c(2006, 2008, 2010, 2012, 2014, 2016, 2018)) %>%
         mutate(Vote = case_when(vv_turnout_gvm==1 ~ 1, vv_turnout_gvm==2 ~ 0),
                State = case_when(state==8 ~ "Colorado", state==37 ~ "North Carolina",
                                  state==35 ~ "New Mexico")) %>% 
         group_by(year, State) %>% summarise(Turnout = mean(Vote)) %>% ungroup() %>%
         filter(State!="New Mexico")


pdf("Paralell.pdf", width=8, height=7)
plot(cces$Turnout ~ cces$year, type="n", xlab="Year", ylab="Voter Turnout", ylim=c(0.4, 1))
lines(cces$Turnout[cces$State=="Colorado"] ~ cces$year[cces$State=="Colorado"], col="gray", lwd=2)
lines(cces$Turnout[cces$State!="Colorado"] ~ cces$year[cces$State!="Colorado"], col="gray", lwd=2, lty=4)
lines(cces$Turnout[cces$State=="Colorado"&cces$year < 2013] ~ cces$year[cces$State=="Colorado"&cces$year < 2013], lwd=2)
lines(cces$Turnout[cces$State!="Colorado"&cces$year < 2013] ~ cces$year[cces$State!="Colorado"&cces$year < 2013], lwd=2, lty=4)
abline(v=2013, lty=3)
arrows(x0=2012.5, y0=0.5, x1=2012.9, y1=0.5, length=0.08) 
text(x=2010.8, y=0.5, labels="VBM Adoption 03'")   
legend("topright", legend=c("Colorado", "North Carolina"), lty=c(1, 4))
dev.off()



# VEP Highest Office Turnout is obtained from (http://www.electproject.org/home/voter-turnout/voter-turnout-data)

year <- rep(seq(from=2000, to=2018, by=2), 2)
vep.colo <- c(57.5, 46.3, 66.7, 47.3, 71.0, 50.6, 69.9, 53.7, 70.0, 61.5)*0.01
vep.nc   <- c(50.7, 39.7, 57.8, 31.0, 65.5, 39.2, 64.8, 40.8, 64.5, 48.7)*0.01
vep.nm   <- c(48.5, 38.6, 59.0, 42.5, 60.9, 42.8, 54.6, 35.4, 54.5, 46.5)*0.01
xtick <- seq(from=2000, to=2018, by=2)


{
pdf("Parallel.pdf", width=12, height=5.5)
par(mar=c(2,2,1,0.5), oma = c(0,3,2,0) + 0.1, mfrow=c(1,2))

# PLOT FOR NORTH CAROLINA
state <- c(rep("Colorado", 10), rep("North Carolina",10))
turnout <- c(vep.colo, vep.nc)
usep <- tibble(year, state, turnout)

plot(usep$turnout ~ usep$year, type="n", xaxt="n", xlab="Year", ylab="Voter turnout", ylim=c(0.3, 1), xlim=c(2000, 2018))
axis(1, at=xtick, las=1, labels=xtick,cex.axis=1, mgp=c(0,0.8,0))
lines(usep$turnout[usep$state=="Colorado"] ~ usep$year[usep$state=="Colorado"], col="gray", lwd=2)
lines(usep$turnout[usep$state!="Colorado"] ~ usep$year[usep$state!="Colorado"], col="gray", lwd=2, lty=2)
lines(usep$turnout[usep$state=="Colorado"&usep$year < 2013] ~ usep$year[usep$state=="Colorado"&usep$year < 2013], lwd=2)
lines(usep$turnout[usep$state!="Colorado"&usep$year < 2013] ~ usep$year[usep$state!="Colorado"&usep$year < 2013], lwd=2, lty=2)
abline(v=2013, lty=1.2, lwd=1.5, col="firebrick4")
arrows(x0=2011.5, y0=0.85, x1=2012.4, y1=0.85, length=0.08) 
text(x=2009, y=0.85, labels="VBM Adoption")   
legend("topleft", legend=c("Colorado", "North Carolina"), cex=1.2, lty=c(1,2), lwd=2, box.lty=0)


# PLOT FOR NEW MEXICO
state <- c(rep("Colorado", 10), rep("New Mexico",10))
turnout <- c(vep.colo, vep.nm)
usep <- tibble(year, state, turnout)

plot(usep$turnout ~ usep$year, type="n", xaxt="n", xlab="Year", ylab="Voter turnout", ylim=c(0.3, 1), xlim=c(2000, 2018))
axis(1, at=xtick, las=1, labels=xtick,cex.axis=1, mgp=c(0,0.8,0))

lines(usep$turnout[usep$state=="Colorado"] ~ usep$year[usep$state=="Colorado"], col="gray", lwd=2)
lines(usep$turnout[usep$state!="Colorado"] ~ usep$year[usep$state!="Colorado"], col="gray", lwd=2, lty=2)
lines(usep$turnout[usep$state=="Colorado"&usep$year < 2013] ~ usep$year[usep$state=="Colorado"&usep$year < 2013], lwd=2)
lines(usep$turnout[usep$state!="Colorado"&usep$year < 2013] ~ usep$year[usep$state!="Colorado"&usep$year < 2013], lwd=2, lty=2)
abline(v=2013, lty=1.2, lwd=1.5, col="firebrick4")
arrows(x0=2011.5, y0=0.85, x1=2012.4, y1=0.85, length=0.08) 
text(x=2009, y=0.85, labels="VBM Adoption")   
legend("topleft", legend=c("Colorado", "New Mexico"), cex=1.2, lty=c(1,2), lwd=2, box.lty=0)
dev.off()
}




# GGPLOT APPROACH
# p <- ggplot(cces, aes(x=year, y=Turnout, color=State)) +
#      geom_point(aes(color=State)) + 
#      geom_line(aes(color=State)) +
#      geom_vline(xintercept = 2013, linetype="dotted", color = "black", size=1) +
#      theme_bw() +
#   #     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#   #          plot.title = element_text(size = 20, face = "bold"),
#   #          legend.position = "none",
#   #          axis.text.x=element_text(size=18, face="bold"),
#   #          axis.text.y=element_blank(),
#   #          axis.ticks.y=element_blank())+
#      xlab("")+ylab("")+ ylim(0.4,1) +  scale_x_continuous(breaks=seq(2006,2018,2))
# p
# 
# 
# 
# # OVERTIME TREND BY GROUPS
# 
# cces2 <- cces %>%
#          filter(race %in% c(1,2,3,4)) %>%
#          group_by(year, State, race) %>% summarise(Turnout = mean(Vote))
# cces2 %>% filter(year==2012 | year==2016) %>% filter(State!="New Mexico")
# cces2 <- cces2 %>% filter(State!="New Mexico")
# 
# p2 <- ggplot(cces2, aes(x=year, y=Turnout, color=State)) +
#      geom_point(aes(color=State)) + 
#      geom_line(aes(color=State)) +
#      geom_vline(xintercept = 2013, linetype="dashed", color = "gray", size=1.5) +
#      facet_wrap(vars(race), nrow = 2) +
# #     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
# #          plot.title = element_text(size = 20, face = "bold"),
# #          legend.position = "none",
# #          axis.text.x=element_text(size=18, face="bold"),
# #          axis.text.y=element_blank(),
# #          axis.ticks.y=element_blank())+
#      xlab("")+ylab("")+ ylim(0.4,1) +  scale_x_continuous(breaks=seq(2006,2018,2))
# p2
################################################################################################
################################################################################################



################################################################################################
# END OF THIS R SOURCE CODE
################################################################################################
  
