################################################################################################
# 15_HypothesisTesting.R
# Created by Yuki Atsusaka
# Since 10/31/2020
# Last updated 10/31/2020
# Aim: to formally conduct a sesries of hypothesis tests
################################################################################################

library(tidyverse)
rm(list=ls())
setwd("R")
nc14 <- read_csv("ATT_NC14.csv")
nc16 <- read_csv("ATT_NC16.csv")
nm14 <- read_csv("ATT_NM14.csv")
nm16 <- read_csv("ATT_NM16.csv")

# NORTH CAROLINA
tau11 <- nc14[2,]
tau12 <- nm14[3,]
tau21 <- nc16[2,]
tau22 <- nm16[3,]


t.test()




################################################################################################
# END OF THIS R SOURCE CODE
################################################################################################
  

