library(dplyr)
library(survey)
source("format_output.r")

asthnow <- read.csv("BRFSS_Dec_Edits/12.11.asthnow_final.csv")

asthnow$ASTHNOW<-relevel(as.factor(asthnow$ASTHNOW), ref='0')
asthnow$RACE <- relevel(asthnow$RACE, ref='white')
asthnow$INCOME <- relevel(asthnow$INCOME, ref='high')
asthnow$EDUCA <- relevel(asthnow$EDUCA, ref='higherEducation')
asthnow$SEX <- relevel(asthnow$SEX, ref='male')
asthnow$SMOKER <- relevel(asthnow$SMOKER, ref='never smoked')
asthnow$BMI <- relevel(asthnow$BMI, ref='normal')

str(asthnow)

men <- filter(asthnow, SEX == "male")
women <- filter(asthnow, SEX == "female")

##weighted models
options(survey.lonely.psu = "adjust")

des.men <- svydesign(ids=~1, strata=~STSTR, weights=~as.numeric(CNTYWT),  data=men)
model.men.weighted <- svyglm(ASTHNOW ~ AGE + EDUCA + INCOME + SMOKER + BMI + RACE, des.men, family=quasibinomial)

des.women <- svydesign(ids=~1, strata=~STSTR, weights=~as.numeric(CNTYWT),  data=women)
model.women.weighted <- svyglm(ASTHNOW ~ AGE + EDUCA + INCOME + SMOKER + BMI + RACE, des.women, family=quasibinomial)

men.output.weighted <- format_output(model.men.weighted)
women.output.weighted <- format_output(model.women.weighted)

weighted.output <- cbind(men.output.weighted, women.output.weighted)
colnames(weighted.output) <- c("male_weighted", "female_weighted")

all <- weighted.output

reordered <- rbind(all[2,], "", "", all[c(14, 16, 13, 15),], "", all[c(3,4),], "", "", all[c(5,6),], "", "", "", all[c(12,9,10,11),], "", "", all[c(8,7),])
adjusted <- reordered

final <- cbind(adjusted[,1], adjusted[,2])

colnames(final) <- c("Men_Adjusted", "Women_Adjusted")
variables <- names(coefficients(model.men.weighted))
varnames <- c(variables[2], "", "", variables[c(14, 16, 13, 15)], "", variables[c(3,4)], "", "", variables[c(5,6)], "", "", "", variables[c(12,9,10,11)], "", "", variables[c(8,7)])
rownames(final) <- varnames


write.csv(final, "BRFSS_Dec_Edits/12.11.table4.csv")




