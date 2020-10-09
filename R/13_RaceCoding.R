
###############################################################################################
# wru sample merge 2 R
# Created by Yuki Atsusaka
# Last updated: July 1, 2018
# Since: June 15, 2018
# Aim: to predict voters' race by wru package and merge the data with voter history file
#      by using "effective date of registration"
##############################################################################################
# MERGING AUGUST 8, 2018

setwd("C:/Users/YUKI/Desktop/Geocoding_Census")
df <- read.table("Second_GeocodedID.txt", sep=",", fill=T, header=T, quote="") # CHANGE THE NAME OF TXT FILE!
df2 <- read.table("full_reg.csv", sep=",", fill=T, header=T, quote="") # CHANGE THE NAME OF TXT FILE!
df3 <- merge(df, df2, by.x="voter_ID", by.y="voter_id", all.x=T)
df4 <- subset(df3, is.na(df3$last_name)==F)
only.c <- subset(df4, is.na(df4$block)==T)
only.b <- subset(df4, is.na(df4$block)==F)

write.table(only.c, file="Second_Regis_County.txt",
            sep=",", col.names=T, append=F, quote=F, row.names=F)
write.table(only.b, file="Second_Regis_Block.txt",
            sep=",", col.names=T, append=F, quote=F, row.names=F)


#(REPEAT THE CODE BELOW FOR ALL DATASETS THAT I LOAD)

rm(list=ls()); gc(); gc()
library(foreign)
library(wru)
library(Hmisc)
library(data.table)
library(dplyr)
library(bigmemory)

###################################################################################################################    
# 0. OUT OF LOOP PREPARATION (DRAWING CENSUS INFORMATION FOR wru PREDICTION)
##################################################################################################################    
# OBTAIN ONE'S OWN API KEY FROM https://api.census.gov/data/key_signup.html

# DOWNLOAD THE CENSUS DATA (COUNTY LEVEL): IF BLOCK GROUP INFORMATION IS AVAILABLE, USE "block" INSTEAD OF "county" 
Cen.county <- get_census_data(key = "6ed7597beb9cace157dc97dbf52976dfbdf2c420",
                             states = "CO", age = F, sex = F, census.geo = "county")

Sys.time()
Cen.block <- get_census_data(key = "6ed7597beb9cace157dc97dbf52976dfbdf2c420",
                             states = "CO", age = T, sex = T, census.geo = "block")
Sys.time() # THIS TAKES ABOUT 1 HOUR


Sys.time()
Cen.block2 <- get_census_data(key = "6ed7597beb9cace157dc97dbf52976dfbdf2c420",
                             states = "CO", age = F, sex = F, census.geo = "block")
Sys.time()


# NOTES: (1) THIS TAKES TIME FOR BLOCK GROUP DATA, 
#        (2) THIS MAY FAIL TO DOWNLAD CENSUS INFORMATION. IF FAILED, PLEASE TRY AGAIN.
#        THE EXAMPLE USES YUKI'S API KEY, PLEASE USE YOURS 

# ESTIMATE VOTERS' RACE BASED ON KHANNA AND IMAI'S (2015) METHOD  

###################################################################################################################    
# 1. DATA READING (REPEAT THE CODE BELOW FOR ALL DATASETS THAT I LOAD)
##################################################################################################################    
# DUE TO ERROS, SKIP THE HEADER AND MANUALLY ADD COLUMN NAMES

  setwd("C:/Users/YUKI/Desktop/Geocoding_Census")
  df <- read.table("Second_Regis_County.txt", sep=",", fill=T, header=T, quote="") # CHANGE THE NAME OF TXT FILE!
                                                                  # Regis_County, Regis_Block1, Regis_Block2, Regis_Block3
# WE DON'T HAVE ANY INFORMATION FOR 117402 REGISTRANTS, ONLY voter_ID  
    df <- df[, c(1,6,7)]
    df.surname <- subset(df, df$county=="")
    df <- subset(df, df$county!="")
    
#  df <- subset(df, is.na(df$COUNTY_CODE)==F)
  colnames(df) <- c("VOTER_ID", "COUNTY_NAME", "surname")  

# CREATE NECCESARY VARIABLES USED IN THE PREDICTION COMMAND 
 df$surname <- tolower(df$surname)
 df$surname <- capitalize(df$surname)
 df$state <- as.character("CO")
 
 df$county <- NA
 
 df$county[df$COUNTY_NAME=="Adams"] <- "001"
 df$county[df$COUNTY_NAME=="Alamosa"] <- "003"
 df$county[df$COUNTY_NAME=="Arapahoe"] <- "005"
 df$county[df$COUNTY_NAME=="Archuleta"] <- "007"
 df$county[df$COUNTY_NAME=="Baca"] <- "009"
 df$county[df$COUNTY_NAME=="Bent"] <- "011"
 df$county[df$COUNTY_NAME=="Boulder"] <- "013"
 df$county[df$COUNTY_NAME=="Chaffee"] <- "015"
 df$county[df$COUNTY_NAME=="Cheyenne"] <- "017"
 df$county[df$COUNTY_NAME=="Clear Creek"] <- "019"
 df$county[df$COUNTY_NAME=="Conejos"] <- "021"
 df$county[df$COUNTY_NAME=="Costilla"] <- "023"
 df$county[df$COUNTY_NAME=="Crowley"] <- "025"
 df$county[df$COUNTY_NAME=="Custer"] <- "027"
 df$county[df$COUNTY_NAME=="Delta"] <- "029"
 df$county[df$COUNTY_NAME=="Denver"] <- "031"
 df$county[df$COUNTY_NAME=="Dolores"] <- "033"
 df$county[df$COUNTY_NAME=="Douglas"] <- "035"
 df$county[df$COUNTY_NAME=="Eagle"] <- "037"
 df$county[df$COUNTY_NAME=="El Paso"] <- "039"
 df$county[df$COUNTY_NAME=="Elbert"] <- "041"
 df$county[df$COUNTY_NAME=="Fremont"] <- "043"
 df$county[df$COUNTY_NAME=="Garfield"] <- "045"
 df$county[df$COUNTY_NAME=="Gilpin"] <- "047"
 df$county[df$COUNTY_NAME=="Grand"] <- "049"
 df$county[df$COUNTY_NAME=="Gunnison"] <- "051"
 df$county[df$COUNTY_NAME=="Hinsdale"] <- "053"
 df$county[df$COUNTY_NAME=="Huerfano"] <- "055"
 df$county[df$COUNTY_NAME=="Jackson"] <- "057"
 df$county[df$COUNTY_NAME=="Jefferson"] <- "059"
 df$county[df$COUNTY_NAME=="Kiowa"] <- "061"
 df$county[df$COUNTY_NAME=="Kit Carson"] <- "063"
 df$county[df$COUNTY_NAME=="Lake"] <- "065"
 df$county[df$COUNTY_NAME=="La Plata"] <- "067"
 df$county[df$COUNTY_NAME=="Larimer"] <- "069"
 df$county[df$COUNTY_NAME=="Las Animas"] <- "071"
 df$county[df$COUNTY_NAME=="Lincoln"] <- "073"
 df$county[df$COUNTY_NAME=="Logan"] <- "075"
 df$county[df$COUNTY_NAME=="Mesa"] <- "077"
 df$county[df$COUNTY_NAME=="Mineral"] <- "079"
 df$county[df$COUNTY_NAME=="Moffat"] <- "081"
 df$county[df$COUNTY_NAME=="Montezuma"] <- "083"
 df$county[df$COUNTY_NAME=="Montrose"] <- "085"
 df$county[df$COUNTY_NAME=="Morgan"] <- "087"
 df$county[df$COUNTY_NAME=="Otero"] <- "089"
 df$county[df$COUNTY_NAME=="Ouray"] <- "091"
 df$county[df$COUNTY_NAME=="Park"] <- "093"
 df$county[df$COUNTY_NAME=="Phillips"] <- "095"
 df$county[df$COUNTY_NAME=="Pitkin"] <- "097"
 df$county[df$COUNTY_NAME=="Prowers"] <- "099"
 df$county[df$COUNTY_NAME=="Pueblo"] <- "101"
 df$county[df$COUNTY_NAME=="Rio Blanco"] <- "103"
 df$county[df$COUNTY_NAME=="Rio Grande"] <- "105"
 df$county[df$COUNTY_NAME=="Routt"] <- "107"
 df$county[df$COUNTY_NAME=="Saguache"] <- "109"
 df$county[df$COUNTY_NAME=="San Juan"] <- "111"
 df$county[df$COUNTY_NAME=="San Miguel"] <- "113"
 df$county[df$COUNTY_NAME=="Sedgwick"] <- "115"
 df$county[df$COUNTY_NAME=="Summit"] <- "117"
 df$county[df$COUNTY_NAME=="Teller"] <- "119"
 df$county[df$COUNTY_NAME=="Washington"] <- "121"
 df$county[df$COUNTY_NAME=="Weld"] <- "123"
 df$county[df$COUNTY_NAME=="Yuma"] <- "125"
 df$county[df$COUNTY_NAME=="Broomfield"] <- "014"
 
 mean(nchar(df$county));typeof(df$county) # CHECK ALL ROW HAS THREE CHARACTERS
 df.county <- df[, c(2,8)]; rm(df) # SECOND TIME

###################################################################################################################    
# 2. "wru" RACE PREDICTION
##################################################################################################################  

 pred.county <- predict_race(df, census.geo = "county", census.data = Cen.county,
                            surname.year=2010) 
 
###################################################################################################################    
# 3. RACE CODING (A)
##################################################################################################################    
# GIVEN THE SET OF PROBABILITIES, WE NEED TO DECIDE HOW TO CODE A PERSON'S RACE
# ONE SIMPLE WAY IS TO ASSIGN A RACE WITH THE HIGHEST PROBABILITY
 
# COUNTY LEVEL ESTIMATION 
 est.c <- pred.county[, 6:10] # ONLY EXTRACT RACE PROBABILITIES TO SPEED UP ALGORITHM
 est.c$highest <- as.numeric(apply(est.c, 1, which.max)) # WHICH COLUMN HAS THE HIGHEST PROBABILITY?
 est.c$race <- ifelse(est.c$highest==1, "White", NA)     # ASSIGN MOST LIKELY RACE
 est.c$race[is.na(est.c$race)==T] <- ifelse(est.c$highest[is.na(est.c$race)==T]==2, "Black", NA)
 est.c$race[is.na(est.c$race)==T] <- ifelse(est.c$highest[is.na(est.c$race)==T]==3, "Hispanic", NA)
 est.c$race[is.na(est.c$race)==T] <- ifelse(est.c$highest[is.na(est.c$race)==T]==4, "Asian", NA)
 est.c$race[is.na(est.c$race)==T] <- ifelse(est.c$highest[is.na(est.c$race)==T]==5, "Others", NA)
 
 hist(est.c$highest, main="Histogram of Estimated Race", xlab="1=White, 2=Black, 3=Hispanic, 4=Asian, 5=Others")  
 table(est.c$race)
 # CHECK THE DISTRIBUTION
 # THE PICTURE LOOKS CONSISTENT WITH COLORADO DEMOGRAPHICS
 df$est.race <- est.c$race # FINALLY, ADD ESTIMATED RACE TO THE ORIGINAL DATA FRAME
 head(df) # CHECK THE LAST COLUMN 
 mean(is.na(df$est.race)==F) # 1 for no age, no gender, no party 

# ONLY KEEP NECESSARY INFORMATION 
 df <- df[, c(1,6)]
 df$est.level <- "county_only" 
 head(df)

 
#-----------------------------------------------------------------------------------#  
 write.table(df, file="Second_voterID_race1.txt",
             sep=",", col.names=T, append=F, quote=F, row.names=F)
#-----------------------------------------------------------------------------------#
 
###################################################################################################################    
# 3. RACE CODING (B)
##################################################################################################################    
###################################################################################################################    
# 1. DATA READING (REPEAT THE CODE BELOW FOR ALL DATASETS THAT I LOAD)
##################################################################################################################    
 # DUE TO ERROS, SKIP THE HEADER AND MANUALLY ADD COLUMN NAMES
 Sys.time()
 setwd("C:/Users/YUKI/Desktop/Geocoding_Census")
 df <- read.table("Second_Regis_Block.txt", sep=",", fill=T, quote="", header=T) # CHANGE THE NAME OF TXT FILE!
 # Regis_County, Regis_Block1, Regis_Block2, Regis_Block3
 df <- df[, c(1,3,4,6,7,24,25,26)]

 colnames(df) <- c("VOTER_ID", "tract", "block","COUNTY_NAME", "surname","PARTY","GENDER","BIRTH_YEAR")
 
 # CREATE NECCESARY VARIABLES USED IN THE PREDICTION COMMAND 
 df$surname <- tolower(df$surname)
 df$surname <- capitalize(df$surname)
 df$state <- as.character("CO")
 typeof(df$state)

 df$county <- NA

 df$county[df$COUNTY_NAME=="Adams"] <- "001"
 df$county[df$COUNTY_NAME=="Alamosa"] <- "003"
 df$county[df$COUNTY_NAME=="Arapahoe"] <- "005"
 df$county[df$COUNTY_NAME=="Archuleta"] <- "007"
 df$county[df$COUNTY_NAME=="Baca"] <- "009"
 df$county[df$COUNTY_NAME=="Bent"] <- "011"
 df$county[df$COUNTY_NAME=="Boulder"] <- "013"
 df$county[df$COUNTY_NAME=="Chaffee"] <- "015"
 df$county[df$COUNTY_NAME=="Cheyenne"] <- "017"
 df$county[df$COUNTY_NAME=="Clear Creek"] <- "019"
 df$county[df$COUNTY_NAME=="Conejos"] <- "021"
 df$county[df$COUNTY_NAME=="Costilla"] <- "023"
 df$county[df$COUNTY_NAME=="Crowley"] <- "025"
 df$county[df$COUNTY_NAME=="Custer"] <- "027"
 df$county[df$COUNTY_NAME=="Delta"] <- "029"
 df$county[df$COUNTY_NAME=="Denver"] <- "031"
 df$county[df$COUNTY_NAME=="Dolores"] <- "033"
 df$county[df$COUNTY_NAME=="Douglas"] <- "035"
 df$county[df$COUNTY_NAME=="Eagle"] <- "037"
 df$county[df$COUNTY_NAME=="El Paso"] <- "039"
 df$county[df$COUNTY_NAME=="Elbert"] <- "041"
 df$county[df$COUNTY_NAME=="Fremont"] <- "043"
 df$county[df$COUNTY_NAME=="Garfield"] <- "045"
 df$county[df$COUNTY_NAME=="Gilpin"] <- "047"
 df$county[df$COUNTY_NAME=="Grand"] <- "049"
 df$county[df$COUNTY_NAME=="Gunnison"] <- "051"
 df$county[df$COUNTY_NAME=="Hinsdale"] <- "053"
 df$county[df$COUNTY_NAME=="Huerfano"] <- "055"
 df$county[df$COUNTY_NAME=="Jackson"] <- "057"
 df$county[df$COUNTY_NAME=="Jefferson"] <- "059"
 df$county[df$COUNTY_NAME=="Kiowa"] <- "061"
 df$county[df$COUNTY_NAME=="Kit Carson"] <- "063"
 df$county[df$COUNTY_NAME=="Lake"] <- "065"
 df$county[df$COUNTY_NAME=="La Plata"] <- "067"
 df$county[df$COUNTY_NAME=="Larimer"] <- "069"
 df$county[df$COUNTY_NAME=="Las Animas"] <- "071"
 df$county[df$COUNTY_NAME=="Lincoln"] <- "073"
 df$county[df$COUNTY_NAME=="Logan"] <- "075"
 df$county[df$COUNTY_NAME=="Mesa"] <- "077"
 df$county[df$COUNTY_NAME=="Mineral"] <- "079"
 df$county[df$COUNTY_NAME=="Moffat"] <- "081"
 df$county[df$COUNTY_NAME=="Montezuma"] <- "083"
 df$county[df$COUNTY_NAME=="Montrose"] <- "085"
 df$county[df$COUNTY_NAME=="Morgan"] <- "087"
 df$county[df$COUNTY_NAME=="Otero"] <- "089"
 df$county[df$COUNTY_NAME=="Ouray"] <- "091"
 df$county[df$COUNTY_NAME=="Park"] <- "093"
 df$county[df$COUNTY_NAME=="Phillips"] <- "095"
 df$county[df$COUNTY_NAME=="Pitkin"] <- "097"
 df$county[df$COUNTY_NAME=="Prowers"] <- "099"
 df$county[df$COUNTY_NAME=="Pueblo"] <- "101"
 df$county[df$COUNTY_NAME=="Rio Blanco"] <- "103"
 df$county[df$COUNTY_NAME=="Rio Grande"] <- "105"
 df$county[df$COUNTY_NAME=="Routt"] <- "107"
 df$county[df$COUNTY_NAME=="Saguache"] <- "109"
 df$county[df$COUNTY_NAME=="San Juan"] <- "111"
 df$county[df$COUNTY_NAME=="San Miguel"] <- "113"
 df$county[df$COUNTY_NAME=="Sedgwick"] <- "115"
 df$county[df$COUNTY_NAME=="Summit"] <- "117"
 df$county[df$COUNTY_NAME=="Teller"] <- "119"
 df$county[df$COUNTY_NAME=="Washington"] <- "121"
 df$county[df$COUNTY_NAME=="Weld"] <- "123"
 df$county[df$COUNTY_NAME=="Yuma"] <- "125"
 df$county[df$COUNTY_NAME=="Broomfield"] <- "014"
 
 mean(nchar(df$county));typeof(df$county) # CHECK ALL ROW HAS THREE CHARACTERS
 
 df$tract[nchar(df$tract)==5] <- paste("0", df$tract[nchar(df$tract)==5], sep="")
 df$tract[nchar(df$tract)==4] <- paste("00", df$tract[nchar(df$tract)==4], sep="")
 df$tract[nchar(df$tract)==3] <- paste("000", df$tract[nchar(df$tract)==3], sep="")
 mean(nchar(df$tract)); typeof(df$tract) # CHECK ALL ROW HAS SIX CHARACTERS
 mean(nchar(df$block)); typeof(df$block) # CHECK ALL ROW HAS SIX CHARACTERS
 df$block <- as.character(df$block); typeof(df$block)


  # WE LOST 824 PEOPLE WITH STAGGERED COLUMNS
 
 df.block <- df[, c(1,2,3,5,9,10)]; rm(df); gc(); gc()
 
# WE DIDN'T LOSE ANY OBSERVATION FROM DF TO DF.BLOCK (8/9/2018)
 
###################################################################################################################    
# 2. "wru" RACE PREDICTION
##################################################################################################################  
################################################################################################################## 

 pred.block <- predict_race(df.block, census.geo = "block", census.data = Cen.block2,
                              surname.year=2010, age=F, sex=F);gc();gc() 

################################################################################################################## 
 
# BLOCK GROUP LEVEL ESTIMATION 
 est <- pred.block[, 7:11] #; rm(pred.block) # ONLY EXTRACT RACE PROBABILITIES TO SPEED UP ALGORITHM
 est$highest <- as.numeric(apply(est, 1, which.max)) # WHICH COLUMN HAS THE HIGHEST PROBABILITY?
 est$race <- ifelse(est$highest==1, "White", NA)     # ASSIGN MOST LIKELY RACE
 est$race[is.na(est$race)==T] <- ifelse(est$highest[is.na(est$race)==T]==2, "Black", NA)
 est$race[is.na(est$race)==T] <- ifelse(est$highest[is.na(est$race)==T]==3, "Hispanic", NA)
 est$race[is.na(est$race)==T] <- ifelse(est$highest[is.na(est$race)==T]==4, "Asian", NA)
 est$race[is.na(est$race)==T] <- ifelse(est$highest[is.na(est$race)==T]==5, "Others", NA)
 
 hist(est$highest, main="Histogram of Estimated Race (Block Group estimates)", xlab="1=White, 2=Black, 3=Hispanic, 4=Asian, 5=Others")
 table(est$race) # CHECK THE NUMBER
 table(est$race[is.na(est$race)==F]) / length(est$race[is.na(est$race)==F]) *100 # CHECK THE PROPORTION
 # CHECK THE DISTRIBUTION
 # THE PICTURE LOOKS CONSISTENT WITH COLORADO DEMOGRAPHICS
 df.block$est.race <- est$race # FINALLY, ADD ESTIMATED RACE TO THE ORIGINAL DATA FRAME
 head(df.block) # CHECK THE LAST COLUMN 
 mean(is.na(df.block$est.race)==F) # 0.8337786 WAS OK
 rm(pred.block); gc(); gc()
 rm(est); gc(); gc()  
 # Create datasets with obs which could not be race coded

 df.resid <- subset(df.block, is.na(df.block$est.race)==T)
 df.block <- subset(df.block, is.na(df.block$est.race)==F)
 df.resid <- df.resid[, c(1:6)]
##################################################################################################################    

 pred.resid <- predict_race(df.resid, census.geo = "county", census.data = Cen.county,
                             surname.year=2010, age=F, sex=F) 

################################################################################################################## 

 est.r <- pred.resid[, 7:11] # ONLY EXTRACT RACE PROBABILITIES TO SPEED UP ALGORITHM
 est.r$highest.r <- as.numeric(apply(est.r, 1, which.max)) # WHICH COLUMN HAS THE HIGHest.r PROBABILITY?
 est.r$race <- ifelse(est.r$highest==1, "White", NA)     # ASSIGN MOST LIKELY RACE
 est.r$race[is.na(est.r$race)==T] <- ifelse(est.r$highest.r[is.na(est.r$race)==T]==2, "Black", NA)
 est.r$race[is.na(est.r$race)==T] <- ifelse(est.r$highest.r[is.na(est.r$race)==T]==3, "Hispanic", NA)
 est.r$race[is.na(est.r$race)==T] <- ifelse(est.r$highest.r[is.na(est.r$race)==T]==4, "Asian", NA)
 est.r$race[is.na(est.r$race)==T] <- ifelse(est.r$highest.r[is.na(est.r$race)==T]==5, "Others", NA)
 
 hist(est.r$highest, main="Histogram of est.rimated Race (County level estimates)", xlab="1=White, 2=Black, 3=Hispanic, 4=Asian, 5=Others")
 table(est.r$race)
 table(est.r$race[is.na(est.r$race)==F]) / length(est.r$race[is.na(est.r$race)==F]) *100 # CHECK THE PROPORTION
 # CHECK THE DISTRIBUTION
 # THE PICTURE LOOKS CONSISTENT WITH COLORADO DEMOGRAPHICS
 df.resid$est.race <- est.r$race # FINALLY, ADD ESTIMATED RACE TO THE ORIGINAL DATA FRAME
 head(df.resid) # CHECK THE LAST COLUMN 
 mean(is.na(df.resid$est.race)==F) # 1
 # WRITE A CODE TO DO A COUNTY LEVEL FOR THOSE WHO WEREN'T RACE CODED

 rm(est.r); rm(pred.resid)
################################################################################################################## 
 


################################################################################################################## 

 df.block <- df.block[, c(1,7)]
 df.county2 <- df.resid[, c(1,7)]

# MERGING ALL VOTER ID WITH ESTIMATED RACE ################################################################### 
 df.block$est.level <- "block_only"
 df.county2$est.level <- "county_only"
 
 voterID_race2 <- bind_rows(df.block, df.county2)
 write.table(voterID_race2, file="voterID_race2.txt",
             sep=",", col.names=T, append=F, quote=F, row.names=F)

# COMBINE TWO OUTPUTS TOGETHER
 
rm(list=ls()) 
 df1 <- read.table("Second_voterID_race1.txt", sep=",", fill=T, header=T, quote="") # COUNTY LEVEL ESTIMATES
 df2 <- read.table("Second_voterID_race2.txt", sep=",", fill=T, header=T, quote="") # COUNTY LEVEL ESTIMATES
 df <- bind_rows(df1, df2)
 write.table(df, file="Second_voterID_race_combined.txt",
             sep=",", col.names=T, append=F, quote=F, row.names=F)
 
  
 
 df_first <- read.table("voterID_race.txt", sep=",", fill=T, header=T, quote="") # COUNTY LEVEL ESTIMATES
 df_com <- bind_rows(df, df_first)
 write.table(df_com, file="voterID_race_recovered.txt",
             sep=",", col.names=T, append=F, quote=F, row.names=F)
 
 
  
 table(voterID_race2$est.race) / length(voterID_race2$est.race) *100 # CHECK THE PROPORTION
 table(voterID_race2$est.race[voterID_race2$est.level=="block"])/length(voterID_race2$est.race[voterID_race2$est.level=="block"]) *100 # CHECK THE PROPORTION
 table(voterID_race2$est.race[voterID_race2$est.level=="county"])/length(voterID_race2$est.race[voterID_race2$est.level=="county"]) *100 # CHECK THE PROPORTION
 
###################################################################################################################    
# MERGE THIS BACK TO 2012-2016 FILE
##################################################################################################################    

 data <- read.table("Reg2012_2016_race_hist_short.txt", sep=",", fill=T, header=T, quote="") # COUNTY LEVEL ESTIMATES

 data  <- read.table("Reg2012_2016_race_hist.txt", sep=",", fill=T, header=T, quote="") # COUNTY LEVEL ESTIMATES
 
 
 
# Oct 3. Check for Writing Report
 
 dat2 <- subset(data, data$est.level!="")
 dat2 <- subset(dat2, dat2$est.level!="2012")
 dat2 <- subset(dat2, dat2$est.level!="2016")
 dat2 <- subset(dat2, dat2$est.level!="Absentee Mail")
 dat2 <- subset(dat2, dat2$est.level!="In Person - DRE")
 dat2 <- subset(dat2, dat2$est.level!="Polling Place Vote Center") 
 dat2 <- subset(dat2, dat2$est.level!="Vote Center - DRE") 
 dat2 <- subset(dat2, is.na(dat2$est.level)==F)
 dat2 <- subset(dat2, dat2$est.level!="") 

 
 
   
 head(data)
 
 table(dat2$est.level)
 table(dat2$est.race) / dim(dat2)[1] * 100


 
# Dataset:
#   2010 SF1 100% Data
# Search:Table:
#   P10: RACE FOR THE POPULATION 18 YEARS AND OVER 
 
# Dataset:
#   2010 SF1 100% Data
# Search:Table:
#   P11: HISPANIC OR LATINO, AND NOT HISPANIC OR LATINO BY RACE FOR THE POPULATION 18 YEARS AND OVER 
 
 Census.2010.white.CO <- 3180748 # From P10 - White only   0.8362496
 Census.2010.black.CO <- 145660  # From P10 - Black only 0.03829543
 Census.2010.asian.CO <- 105569 # From P10 - Asian only 0.02775512
 Census.2010.hispn.CO <- 664462 # From P11 - Hispanic 0.1746935
 total <- 3803587
 
 
 white <- 5223656  # 74.34%
 black <- 664117   # 9.45%
 asian <- 343432   # 4.89%
 hispanic <- 734456  # 10.45%
 others <- 60833 # 0.87%
 
  
###################################################################################################################    
# END OF THIS R SOURCE FILE
##################################################################################################################    

 
