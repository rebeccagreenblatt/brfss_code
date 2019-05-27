library(dplyr)
library(survey)
source("format_output.r")

##current asthma
asthnow <- read.csv("BRFSS_Dec_Edits/12.11.asthnow_final.csv")
str(asthnow)
#relevel
asthnow$ASTHNOW<-relevel(as.factor(asthnow$ASTHNOW), ref='0')
asthnow$RACE <- relevel(asthnow$RACE, ref='white')
asthnow$INCOME <- relevel(asthnow$INCOME, ref='high')
asthnow$EDUCA <- relevel(asthnow$EDUCA, ref='higherEducation')
asthnow$SEX <- relevel(asthnow$SEX, ref='male')
asthnow$SMOKER <- relevel(asthnow$SMOKER, ref='never smoked')
asthnow$BMI <- relevel(asthnow$BMI, ref='normal')

options(survey.lonely.psu = "adjust")
des <- svydesign(ids=~1, strata=~STSTR, weights=~as.numeric(CNTYWT),  data=asthnow)

##unadjusted ORs
age <- svyglm(ASTHNOW ~ AGE, des, family = quasibinomial)
age_results <- format_output(age)[2]

sex <- svyglm(ASTHNOW ~ SEX, des, family = quasibinomial)
sex_results <- format_output(sex)[2]

race <- svyglm(ASTHNOW ~ RACE, des, family = quasibinomial)
race_results <- t(t(format_output(race)[2:5][c(2,4,1,3)]))

educa <- svyglm(ASTHNOW ~ EDUCA, des, family = quasibinomial)
educa_results <- t(t(format_output(educa)[2:3]))

income <- svyglm(ASTHNOW ~ INCOME, des, family = quasibinomial)
income_results <- t(t(format_output(income)[2:3]))

bmi <- svyglm(ASTHNOW ~ BMI, des, family = quasibinomial)
bmi_results <- t(t(format_output(bmi)[2:5][c(4,1,2,3)]))

smoker <- svyglm(ASTHNOW ~ SMOKER, des, family = quasibinomial)
smoker_results <- t(t(format_output(smoker)[2:3][c(2,1)]))

#b <- c(5, 5, 5)
b <- c("")
unadjusted <- rbind(age_results, b, b, sex_results, b, b, race_results, b, educa_results, b, b, income_results, b, b, b, bmi_results, b, b, smoker_results)

unadj_final <- unadjusted

unadj_final[c(3,6,14,18,20,26),] <- rep("Reference", 2)

write.csv(unadj_final, file = "BRFSS_Dec_Edits/12.11.table3_crudeORs.csv")

##weighted asthma prev overall
svymean(~ASTHNOW, des)




