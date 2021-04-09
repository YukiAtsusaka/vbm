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



# Perform the sensitivity analysis via the McNemar's test

dt <- matrix(c(30, 12, 40, 18), nrow = 2,
    dimnames = list("After Video" = c("Support", "Do Not Support"),
                    "Before Video" = c("Support", "Do Not Support")))
mcnemar.test(dt)


################################################################################################
# END OF THIS R SOURCE CODE
################################################################################################
