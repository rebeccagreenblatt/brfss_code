library(dplyr)
library(survey)
source("format_output.r")

asthnow <- read.csv("BRFSS_Dec_Edits/12.11.asthnow_final.csv")

#MEN
men <- filter(asthnow, SEX == "male")

men$ASTHNOW<-relevel(as.factor(men$ASTHNOW), ref='0')
men$RACE <- relevel(men$RACE, ref='white')
men$INCOME <- relevel(men$INCOME, ref='high')
men$EDUCA <- relevel(men$EDUCA, ref='higherEducation')
men$SEX <- relevel(men$SEX, ref='male')
men$SMOKER <- relevel(men$SMOKER, ref='never smoked')
men$BMI <- relevel(men$BMI, ref='normal')

#getting prevalence by year
des.men <- svydesign(ids=~1, strata=~STSTR, weights=~as.numeric(CNTYWT),  data=men)
prev_byyear.men <- round(prop.table(svytable(~YEAR+ASTHNOW, des.men),1)[,2]*100, 2)

str(men)

output.byyear <- sapply(2007:2012, function(year){
  BRFSS.singleyear <- filter(men, YEAR == year)
  des <- svydesign(ids=~1, strata=~STSTR, weights=~as.numeric(CNTYWT),  data=BRFSS.singleyear)
  model <- svyglm(ASTHNOW ~ AGE+EDUCA+INCOME+RACE+SMOKER+BMI, des, family=quasibinomial)
  return(format_output(model))
})

colnames(output.byyear) <- c("2007": "2012")


n_per_year <- men %>% group_by(YEAR) %>%
  summarise(N = n())
yearly_n <- t(as.vector(n_per_year[,2]))
printed <- paste0("Current Asthma ", c("2007": "2012"), paste0("(", "N = ", formatC(yearly_n, format="d",big.mark=","), ")"))

ors <- output.byyear
b <- rep("", 6)
ref <- rep("Reference", 6)
table_form <- rbind(printed, b, ors[2,], b, ref, ors[8,], 
                    ors[10,], ors[7,], ors[9,], b, ors[3,], ors[4,], ref, b,
                    ors[5,], ors[6,], ref, b, ref, ors[16,], ors[13,], ors[14,], ors[15,], b, ref, ors[12,], ors[11,])

rownames(table_form) <- rep("", 27)

write.csv(table_form, file = "BRFSS_Dec_Edits/12.11.men_ors_by_year.csv")


#WOMEN
women <- filter(asthnow, SEX=="female")

women$ASTHNOW<-relevel(as.factor(women$ASTHNOW), ref='0')
women$RACE <- relevel(women$RACE, ref='white')
women$INCOME <- relevel(women$INCOME, ref='high')
women$EDUCA <- relevel(women$EDUCA, ref='higherEducation')
women$SEX <- relevel(women$SEX, ref='male')
women$SMOKER <- relevel(women$SMOKER, ref='never smoked')
women$BMI <- relevel(women$BMI, ref='normal')

#getting prev by year
des.women <- svydesign(ids=~1, strata=~STSTR, weights=~as.numeric(CNTYWT),  data=women)
prev_byyear.women <- round(prop.table(svytable(~YEAR+ASTHNOW, des.women),1)[,2]*100, 2)

str(women)

output.byyear <- sapply(2007:2012, function(year){
  BRFSS.singleyear <- filter(women, YEAR == year)
  des <- svydesign(ids=~1, strata=~STSTR, weights=~as.numeric(CNTYWT),  data=BRFSS.singleyear)
  model <- svyglm(ASTHNOW ~ AGE+EDUCA+INCOME+RACE+SMOKER+BMI, des, family=quasibinomial)
  return(format_output(model))
})

colnames(output.byyear) <- c("2007": "2012")

n_per_year <- women %>% group_by(YEAR) %>%
  summarise(N = n())
yearly_n <- t(as.vector(n_per_year[,2]))
printed <- paste0("Current Asthma ", c("2007": "2012"), paste0("(", "N = ", formatC(yearly_n, format="d",big.mark=","), ")"))

ors <- output.byyear
b <- rep("", 6)
ref <- rep("Reference", 6)
table_form <- rbind(printed, b, ors[2,], b, ref, ors[8,], 
                    ors[10,], ors[7,], ors[9,], b, ors[3,], ors[4,], ref, b,
                    ors[5,], ors[6,], ref, b, ref, ors[16,], ors[13,], ors[14,], ors[15,], b, ref, ors[12,], ors[11,])

rownames(table_form) <- rep("", 27)

write.csv(table_form, file = "BRFSS_Dec_Edits/12.11.women_ors_by_year.csv")


##ALL
asthnow$ASTHNOW<-relevel(as.factor(asthnow$ASTHNOW), ref='0')
asthnow$RACE <- relevel(asthnow$RACE, ref='white')
asthnow$INCOME <- relevel(asthnow$INCOME, ref='high')
asthnow$EDUCA <- relevel(asthnow$EDUCA, ref='higherEducation')
asthnow$SEX <- relevel(asthnow$SEX, ref='male')
asthnow$SMOKER <- relevel(asthnow$SMOKER, ref='never smoked')
asthnow$BMI <- relevel(asthnow$BMI, ref='normal')

#getting prevalence by year
des.asthnow <- svydesign(ids=~1, strata=~STSTR, weights=~as.numeric(CNTYWT),  data=asthnow)
prev_byyear.asthnow <- round(prop.table(svytable(~YEAR+ASTHNOW, des.asthnow),1)[,2]*100, 2)

str(asthnow)

output.byyear <- sapply(2007:2012, function(year){
  BRFSS.singleyear <- filter(asthnow, YEAR == year)
  des <- svydesign(ids=~1, strata=~STSTR, weights=~as.numeric(CNTYWT),  data=BRFSS.singleyear)
  model <- svyglm(ASTHNOW ~ AGE+EDUCA+INCOME+RACE+SMOKER+BMI+SEX, des, family=quasibinomial)
  return(format_output(model))
})

colnames(output.byyear) <- c("2007": "2012")


n_per_year <- asthnow %>% group_by(YEAR) %>%
  summarise(N = n())
yearly_n <- t(as.vector(n_per_year[,2]))
printed <- paste0("Current Asthma ", c("2007": "2012"), paste0("(", "N = ", formatC(yearly_n, format="d",big.mark=","), ")"))

ors <- output.byyear
b <- rep("", 6)
ref <- rep("Reference", 6)
table_form <- rbind(printed, b, ref, ors[17,], b, ors[2,], b, ref, ors[8,], 
                    ors[10,], ors[7,], ors[9,], b, ors[3,], ors[4,], ref, b,
https://mail.google.com/mail/u/0/#inbox                    ors[5,], ors[6,], ref, b, ref, ors[16,], ors[13,], ors[14,], ors[15,], b, ref, ors[12,], ors[11,])

rownames(table_form) <- rep("", 30)

write.csv(table_form, file = "BRFSS_Dec_Edits/all_ors_by_year.csv")

#write.csv(rbind(prev_byyear.men, prev_byyear.women, prev_byyear.asthnow), "weighted_prev_by_year_by_sex_0712.csv")

