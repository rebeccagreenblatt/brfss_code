library(car)
library(dplyr)

BRFSS.complete.allvars <- read.csv("BRFSS_Dec_Edits/12.11.brfss.complete.csv")

BRFSS.complete.allvars$STSTR <- BRFSS.complete.allvars$CntyFIPS 
names(BRFSS.complete.allvars)[names(BRFSS.complete.allvars)=="ASTHMA2"] <- "ASTHMA"
names(BRFSS.complete.allvars)[names(BRFSS.complete.allvars)=="INCOME2"] <- "INCOME"
names(BRFSS.complete.allvars)[names(BRFSS.complete.allvars)=="RACE2"] <- "RACE"
names(BRFSS.complete.allvars)[names(BRFSS.complete.allvars)=="BMICAT"] <- "BMI"
names(BRFSS.complete.allvars)[names(BRFSS.complete.allvars)=="id"] <- "YEAR"

BRFSS.complete <- select(BRFSS.complete.allvars, ASTHMA, ASTHNOW, AGE, EDUCA, INCOME, SEX, SMOKER, BMI, CNTYWT, CntyFIPS, RACE, STSTR, YEAR)

BRFSS.complete$ASTHMA<-recode(BRFSS.complete$ASTHMA, recodes="1='1'; 2='0';c(7,9)=NA", as.factor.result=T)
BRFSS.complete$ASTHMA<-relevel(BRFSS.complete$ASTHMA, ref='0')

BRFSS.complete <- filter(BRFSS.complete, YEAR %in% c(2007:2012))

BRFSS.complete$ASTHNOW<-ifelse(BRFSS.complete$ASTHMA == 0, 0, BRFSS.complete$ASTHNOW) 
BRFSS.complete <- filter(BRFSS.complete, ASTHNOW != 2 )
BRFSS.complete$ASTHNOW<-recode(BRFSS.complete$ASTHNOW, recodes="1='1';0='0';else=NA", as.factor.result=T) #1, 2,7,9 add up to people who have ever had asthma
BRFSS.complete$ASTHNOW<-relevel(BRFSS.complete$ASTHNOW, ref='0')

#Race/ethnicity
BRFSS.complete$RACE <- recode(BRFSS.complete$RACE, recodes="1='white'; 2='black'; 3:4='asian/pacific islander'; 5='native'; 8='hispanic'; c(6,7,9,0)=NA", as.factor.result=T)
BRFSS.complete$RACE <- relevel(BRFSS.complete$RACE, ref='white')

#Income <$25K, $25-75K, >$75k
BRFSS.complete$INCOME <- recode(BRFSS.complete$INCOME, recodes="1:4='low'; 5:7='middle'; 8='high'; c(0,77,99)=NA", as.factor.result=T)
BRFSS.complete$INCOME <- relevel(BRFSS.complete$INCOME, ref='high')

#Age
BRFSS.complete$AGE <- recode(BRFSS.complete$AGE, recodes="0:9=NA", as.numeric.result=T) #come back to this to talk about missing data
BRFSS.complete <- filter(BRFSS.complete, AGE > 21 | is.na(AGE)) ##new line (just removed people from 18 through 21)

##dim(BRFSS.complete)[1] #1970066 (cut out 48125 subjects ages 18-21)

#Education
BRFSS.complete$EDUCA <- recode(BRFSS.complete$EDUCA, recodes="1:2='noHighSchool'; 3:4='Some/highSchool'; 5:6='higherEducation';c(0,9)=NA", as.factor.result=T)
BRFSS.complete$EDUCA <- relevel(BRFSS.complete$EDUCA, ref='higherEducation')

#Sex
BRFSS.complete$SEX <- recode(BRFSS.complete$SEX, recodes="1='male'; 2='female'", as.factor.result=T)
BRFSS.complete$SEX <- relevel(BRFSS.complete$SEX, ref='male')

#Smoking
BRFSS.complete$SMOKER <- recode(BRFSS.complete$SMOKER, recodes="1:2='current smoker'; 3='former smoker'; 4='never smoked';9=NA", as.factor.result=T)
BRFSS.complete$SMOKER <- relevel(BRFSS.complete$SMOKER, ref='never smoked')

#BMI
BRFSS.complete$BMI <- relevel(BRFSS.complete$BMI, ref='normal')


BRFSS.all <- BRFSS.complete #1,970,066 subjects

asthnow <- select(BRFSS.complete, ASTHNOW, AGE, EDUCA, INCOME, SEX, SMOKER, BMI, CNTYWT, CntyFIPS, RACE, STSTR, YEAR)
asthnow.final <- asthnow[complete.cases(asthnow),] #1502655
missing_asthnow <- asthnow[rowSums(is.na(asthnow)) > 0, ] #467411

write.csv(asthnow.final, "BRFSS_Dec_Edits/12.11.asthnow_final.csv")
write.csv(missing_asthnow, "BRFSS_Dec_Edits/12.11.asthnow_missing.csv")
