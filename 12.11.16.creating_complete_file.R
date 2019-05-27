library(dplyr)

for (i in 2002:2012){
  assign(paste0("brfss.",i), read.csv(paste("census-app/data/Cnty_BRFSS_",i,".C.csv", sep="")))
}

##add asthnow to 2003 file.. 
asthnow.2003 <- read.csv("ASTHNOW_2003_ONLY.csv")
brfss.2003$ASTHNOW <- asthnow.2003$ASTHNOW
##checks
table(brfss.2003[!is.na(brfss.2003$ASTHNOW),]$ASTHMA2, useNA = "ifany") #should only be 1 - check
table(brfss.2003[is.na(brfss.2003$ASTHNOW),]$ASTHMA2, useNA = "ifany") #should only be 2, 7 or 9

yearly.brfss <- list(brfss.2002, brfss.2003, brfss.2004, 
                     brfss.2005, brfss.2006, brfss.2007, 
                     brfss.2008, brfss.2009, brfss.2010, 
                     brfss.2011, brfss.2012)

for (i in yearly.brfss){
  i <- tbl_df(i)
}

##looking at CNTYWT 
for (i in yearly.brfss){
  if("F__CNTYWT" %in% colnames(i)) {print("includes CNTYWT var")}
  else {print("no, does not have CNTYWT var")}
}
##2002 is called A_CNTYWT
brfss.2002$CNTYWT <- as.numeric(gsub(",","", as.character(brfss.2002$A_CNTYWT)))
brfss.2003$CNTYWT <- as.numeric(gsub(",","", as.character(brfss.2003$F__CNTYWT)))
brfss.2004$CNTYWT <- as.numeric(gsub(",","", as.character(brfss.2004$F__CNTYWT)))
brfss.2005$CNTYWT <- as.numeric(gsub(",","", as.character(brfss.2005$F__CNTYWT)))
brfss.2006$CNTYWT <- as.numeric(gsub(",","", as.character(brfss.2006$F__CNTYWT)))
brfss.2007$CNTYWT <- as.numeric(gsub(",","", as.character(brfss.2007$F__CNTYWT)))
brfss.2008$CNTYWT <- as.numeric(gsub(",","", as.character(brfss.2008$F__CNTYWT)))
brfss.2009$CNTYWT <- as.numeric(gsub(",","", as.character(brfss.2009$F__CNTYWT)))
brfss.2010$CNTYWT <- as.numeric(gsub(",","", as.character(brfss.2010$F__CNTYWT)))
brfss.2011$CNTYWT <- as.numeric(gsub(",","", as.character(brfss.2011$F__CNTYWT)))
brfss.2012$CNTYWT <- as.numeric(gsub(",","", as.character(brfss.2012$F__CNTYWT)))

##looking at whether they have asthma now
for (i in yearly.brfss){
  if("ASTHNOW" %in% colnames(i)) {print("includes ASTHNOW var")}
  else {print("no, does not have ASTHNOW var")}
}

for (i in yearly.brfss){
  if("HISPANC2" %in% colnames(i)) {print("includes HISPANC2 var")}
  else {print("no, does not have ASTHNOW var")}
}

#1=yes, 2=no, 7=Don't know/Not sure, 9=Refused, NA is for people who have never had asthma, so will count them as NOs
table(brfss.2002$ASTHMA2, useNA = "ifany")

#variable name changed to ASTHMA3 from ASTHMA2 in 2011, the questions are almost identical
brfss.2011$ASTHMA2 <- brfss.2011$ASTHMA3
brfss.2012$ASTHMA2 <- brfss.2012$ASTHMA3

##SMOKER
for (i in yearly.brfss){
  if("F__SMOKER3" %in% colnames(i)) {print("includes SMOKER var")}
  else {print("no, does not have SMOKER var")}
}
##2002: variable is named A_SMOKER
##2003-2004: variable is F__SMOKER2
##2005-2012: variable is F__SMOKER3

#renaming all to smoker
brfss.2002$SMOKER <- brfss.2002$A_SMOKER
brfss.2003$SMOKER <- brfss.2003$F__SMOKER2
brfss.2004$SMOKER <- brfss.2004$F__SMOKER2
brfss.2005$SMOKER <- brfss.2005$F__SMOKER3
brfss.2006$SMOKER <- brfss.2006$F__SMOKER3
brfss.2007$SMOKER <- brfss.2007$F__SMOKER3
brfss.2008$SMOKER <- brfss.2008$F__SMOKER3
brfss.2009$SMOKER <- brfss.2009$F__SMOKER3
brfss.2010$SMOKER <- brfss.2010$F__SMOKER3
brfss.2011$SMOKER <- brfss.2011$F__SMOKER3
brfss.2012$SMOKER <- brfss.2012$F__SMOKER3

##making our own BMI variable based on actual values...
#variable name different for each year
brfss.2002$BMIVAL <- as.numeric(gsub(",","", as.character(brfss.2002$A_BMI2)))
brfss.2003$BMIVAL <- as.numeric(gsub(",","", as.character(brfss.2003$F__BMI3)))
brfss.2004$BMIVAL <- as.numeric(gsub(",","", as.character(brfss.2004$F__BMI4)))
brfss.2005$BMIVAL <- as.numeric(gsub(",","", as.character(brfss.2005$F__BMI4)))
brfss.2006$BMIVAL <- as.numeric(gsub(",","", as.character(brfss.2006$F__BMI4)))
brfss.2007$BMIVAL <- as.numeric(gsub(",","", as.character(brfss.2007$F__BMI4)))
brfss.2008$BMIVAL <- as.numeric(gsub(",","", as.character(brfss.2008$F__BMI4)))
brfss.2009$BMIVAL <- as.numeric(gsub(",","", as.character(brfss.2009$F__BMI4)))
brfss.2010$BMIVAL <- as.numeric(gsub(",","", as.character(brfss.2010$F__BMI4)))
brfss.2011$BMIVAL <- as.numeric(gsub(",","", as.character(brfss.2011$F__BMI5)))
brfss.2012$BMIVAL <- as.numeric(gsub(",","", as.character(brfss.2012$F__BMI5)))
##NA vals are different for each year...
#ex: 2002-101 --> 9999, 2011-2012 --> 0

variables.to.keep <- c("ASTHMA2", "ASTHNOW", "SMOKER", "AGE", "EDUCA",
                       "INCOME2", "SEX", "RACE2", "BMIVAL", "CntyFIPS", "CNTYWT") 

reduced.2002 <- brfss.2002[,variables.to.keep]
reduced.2003 <- brfss.2003[,variables.to.keep]
reduced.2004 <- brfss.2004[,variables.to.keep]
reduced.2005 <- brfss.2005[,variables.to.keep]
reduced.2006 <- brfss.2006[,variables.to.keep]
reduced.2007 <- brfss.2007[,variables.to.keep]
reduced.2008 <- brfss.2008[,variables.to.keep]
reduced.2009 <- brfss.2009[,variables.to.keep]
reduced.2010 <- brfss.2010[,variables.to.keep]
reduced.2011 <- brfss.2011[,variables.to.keep]
reduced.2012 <- brfss.2012[,variables.to.keep]

##adding IDs to datasets so when we combine we keep what year the data was from
reduced.2002$id <- 2002
reduced.2003$id <- 2003
reduced.2004$id <- 2004
reduced.2005$id <- 2005
reduced.2006$id <- 2006
reduced.2007$id <- 2007
reduced.2008$id <- 2008
reduced.2009$id <- 2009
reduced.2010$id <- 2010
reduced.2011$id <- 2011
reduced.2012$id <- 2012

yearly.reduced <- list(reduced.2002, reduced.2003, reduced.2004,
                       reduced.2005, reduced.2006, reduced.2007,
                       reduced.2008, reduced.2009, reduced.2010,
                       reduced.2011, reduced.2012)

##combien all years into one data table
library(data.table)
brfss.all.years <- rbindlist(yearly.reduced)

##converting CntyFIPS to integer
brfss.all.years$CntyFIPS <- as.numeric(gsub(",","", as.character(brfss.all.years$CntyFIPS)))

##converting BMIVAL to show NA values
brfss.all.years$BMIVAL <- ifelse(brfss.all.years$BMIVAL > 0 & brfss.all.years$BMIVAL < 9999, brfss.all.years$BMIVAL, NA)
#check
table(is.na(brfss.all.years$BMIVAL))
tot <- 0
for (i in yearly.reduced){
  tot = tot + sum(i$BMIVAL == 0 | i$BMIVAL == 9999)
}

library(car)

#12.11
brfss.all.years$BMICAT <- recode(brfss.all.years$BMIVAL, recodes = ("1:2499 = 'normal'; 2500:2999= 'overweight'; 3000:3499= 'grade 1 obesity'; 3500:3999 = 'grade 2 obesity'; 4000:9998= 'grade 3 obesity'"), as.factor.result = TRUE)


#12.11
write.csv(brfss.all.years, file = "BRFSS_Dec_Edits/12.11.brfss.complete.csv")


