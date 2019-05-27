library(ggplot2)
library(dplyr)
library(survey)
library(reshape2)

#current <- read.csv("asthnow_final_1_19.csv")
current <- read.csv("MAY_edits/asthnow_final_0712.csv")

current$ASTHNOW<-relevel(as.factor(current$ASTHNOW), ref='0')
current$RACE <- relevel(current$RACE, ref='white')
current$INCOME <- relevel(current$INCOME, ref='high')
current$EDUCA <- relevel(current$EDUCA, ref='higherEducation')
current$SEX <- relevel(current$SEX, ref='male')
current$SMOKER <- relevel(current$SMOKER, ref='never smoked')
current$BMI <- relevel(current$BMI, ref='normal')

levels(current$RACE) <- c("White", "Asian/Pacific Islander", 
                           "Black", "Hispanic", "American Indian/Alaskan Native")
current$INCOME = factor(current$INCOME,levels(current$INCOME)[c(2,3,1)])
current$EDUCA = factor(current$EDUCA,levels(current$EDUCA)[c(2,3,1)])
current$SMOKER = factor(current$SMOKER,levels(current$SMOKER)[c(1,3,2)])
current$BMI = factor(current$BMI,levels(current$BMI)[c(1,4,2,3)])
current$AGE_GROUP <- cut(current$AGE, breaks = c(21, 34, 44, 54, 64, 100))
levels(current$AGE_GROUP) <- c("22-34", "35-44", "45-54", "55-64", "65+")
levels(current$INCOME) <- c("<$25,000", "$25,000-$75,000", ">$75,000")
levels(current$EDUCA) <- c("Less than high school", "High school", "Some college or more")
levels(current$SMOKER) <- c("Never smoked", "Former smoker", "Current smoker")
levels(current$BMI) <- c("Not overweight or obese", "Overweight", "Grade 1 Obese", "Grades 2 & 3 Obese")
levels(current$SEX) <- c("Male", "Female")

des_current <- svydesign(ids=~1, strata=~STSTR, weights=~as.numeric(CNTYWT),  data=current)

##Current Asthma Prevalence

asthnow <- as.data.frame(svytable(~ASTHNOW+CntyFIPS+ YEAR, des_current))
asthnow_wide <- dcast(asthnow, CntyFIPS + YEAR ~ ASTHNOW, value.var = "Freq")
asthnow_wide$total <- rowSums(asthnow_wide[,c(-1, -2)])
asthnow_wide[,c(-1,-2)] <- asthnow_wide[,c(-1,-2)]/asthnow_wide$total
asthnow_final <- asthnow_wide[complete.cases(asthnow_wide),-ncol(asthnow_wide)]
colnames(asthnow_final) <- c("CntyFIPS", "YEAR", "no_asthnow", "asthnow")

asthnow_all <- as.data.frame(svytable(~ASTHNOW+CntyFIPS , des_current))
asthnow_all_wide <- dcast(asthnow_all, CntyFIPS   ~ ASTHNOW, value.var = "Freq")
asthnow_all_wide$total <- rowSums(asthnow_all_wide[,c(-1)])
asthnow_all_wide[,c(-1)] <- asthnow_all_wide[,c(-1)]/asthnow_all_wide$total
asthnow_all_final <- asthnow_all_wide[complete.cases(asthnow_all_wide),-ncol(asthnow_all_wide)]
colnames(asthnow_all_final) <- c("CntyFIPS", "no_asthnow", "asthnow")
asthnow_all_final$YEAR <- "all"
asthnow_all_final2 <- asthnow_all_final[,c("CntyFIPS", "YEAR", "no_asthnow", "asthnow")]

weighted_asthnow_dat <- tbl_df(rbind(asthnow_final, asthnow_all_final2))
weighted_asthnow_dat$CntyFIPS <- as.numeric(as.character(weighted_asthnow_dat$CntyFIPS))

counts <- current %>% group_by(CntyFIPS, YEAR) %>% summarise(count = n())
counts_all <- current %>% group_by(CntyFIPS) %>% summarise(YEAR = "all", count = n())
counts_final <- rbind(counts, counts_all)

weighted_asthnow_final <- inner_join(weighted_asthnow_dat, counts_final, by = c("CntyFIPS", "YEAR"))
weighted_asthnow_final2 <- select(weighted_asthnow_final, CntyFIPS, YEAR, asthnow, count)

#write.csv(weighted_asthnow_dat, file = "weighted_asthnow_dat.csv")
#write.csv(weighted_asthnow_final2, file = "weighted_current_asthma_w_counts.csv")
write.csv(weighted_asthnow_final2, file = "leaflet_app/data/weighted_current_asthma_w_counts_leaflet.csv")

######all variables
income <- as.data.frame(svytable(~INCOME+CntyFIPS+ YEAR + ASTHNOW, des_current))
income_wide <- dcast(income, CntyFIPS + YEAR +ASTHNOW ~ INCOME, value.var = "Freq")
income_wide$total <- rowSums(income_wide[,c(-1, -2, -3)])
income_wide[,c(-1,-2, -3)] <- income_wide[,c(-1,-2, -3)]/income_wide$total
income_final <- income_wide[complete.cases(income_wide),-ncol(income_wide)]

sex <- as.data.frame(svytable(~SEX+CntyFIPS+ YEAR+ASTHNOW, des_current))
sex_wide <- dcast(sex, CntyFIPS + YEAR +ASTHNOW ~ SEX, value.var = "Freq")
sex_wide$total <- rowSums(sex_wide[,c(-1, -2, -3)])
sex_wide[,c(-1,-2, -3)] <- sex_wide[,c(-1,-2, -3)]/sex_wide$total
sex_final <- sex_wide[complete.cases(sex_wide),c(-1,-2, -3,-ncol(sex_wide))]

race <- as.data.frame(svytable(~RACE+CntyFIPS+ YEAR+ASTHNOW, des_current))
race_wide <- dcast(race, CntyFIPS + YEAR +ASTHNOW ~ RACE, value.var = "Freq")
race_wide$total <- rowSums(race_wide[,c(-1, -2, -3)])
race_wide[,c(-1,-2, -3)] <- race_wide[,c(-1,-2, -3)]/race_wide$total
race_final <- race_wide[complete.cases(race_wide),c(-1,-2, -3,-ncol(race_wide))]

bmi <- as.data.frame(svytable(~BMI+CntyFIPS+ YEAR+ASTHNOW, des_current))
bmi_wide <- dcast(bmi, CntyFIPS + YEAR+ASTHNOW ~ BMI, value.var = "Freq")
bmi_wide$total <- rowSums(bmi_wide[,c(-1, -2,-3)])
bmi_wide[,c(-1,-2,-3)] <- bmi_wide[,c(-1,-2,-3)]/bmi_wide$total
bmi_final <- bmi_wide[complete.cases(bmi_wide),c(-1,-2,-3,-ncol(bmi_wide))]

smoker <- as.data.frame(svytable(~SMOKER+CntyFIPS+ YEAR+ASTHNOW, des_current))
smoker_wide <- dcast(smoker, CntyFIPS + YEAR+ASTHNOW ~ SMOKER, value.var = "Freq")
smoker_wide$total <- rowSums(smoker_wide[,c(-1, -2,-3)])
smoker_wide[,c(-1,-2,-3)] <- smoker_wide[,c(-1,-2,-3)]/smoker_wide$total
smoker_final <- smoker_wide[complete.cases(smoker_wide),c(-1,-2,-3,-ncol(smoker_wide))]

educa <- as.data.frame(svytable(~EDUCA+CntyFIPS+ YEAR+ASTHNOW, des_current))
educa_wide <- dcast(educa, CntyFIPS + YEAR+ASTHNOW ~ EDUCA, value.var = "Freq")
educa_wide$total <- rowSums(educa_wide[,c(-1, -2,-3)])
educa_wide[,c(-1,-2,-3)] <- educa_wide[,c(-1,-2,-3)]/educa_wide$total
educa_final <- educa_wide[complete.cases(educa_wide),c(-1,-2,-3,-ncol(educa_wide))]

age_group <- as.data.frame(svytable(~AGE_GROUP+CntyFIPS+ YEAR+ASTHNOW, des_current))
age_group_wide <- dcast(age_group, CntyFIPS + YEAR+ASTHNOW ~ AGE_GROUP, value.var = "Freq")
age_group_wide$total <- rowSums(age_group_wide[,c(-1, -2,-3)])
age_group_wide[,c(-1,-2,-3)] <- age_group_wide[,c(-1,-2,-3)]/age_group_wide$total
age_group_final <- age_group_wide[complete.cases(age_group_wide),c(-1,-2,-3,-ncol(age_group_wide))]

yearly <- cbind(income_final, sex_final, race_final,
                bmi_final, smoker_final, educa_final, age_group_final)

##all years
income <- as.data.frame(svytable(~INCOME+CntyFIPS+ASTHNOW , des_current))
income_wide <- dcast(income, CntyFIPS+ASTHNOW   ~ INCOME, value.var = "Freq")
income_wide$total <- rowSums(income_wide[,c(-1,-2)])
income_wide[,c(-1,-2)] <- income_wide[,c(-1,-2)]/income_wide$total
income_final <- income_wide[complete.cases(income_wide),-ncol(income_wide)]

sex <- as.data.frame(svytable(~SEX+CntyFIPS+ASTHNOW , des_current))
sex_wide <- dcast(sex, CntyFIPS+ASTHNOW   ~ SEX, value.var = "Freq")
sex_wide$total <- rowSums(sex_wide[,c(-1,-2)])
sex_wide[,c(-1,-2)] <- sex_wide[,c(-1,-2)]/sex_wide$total
sex_final <- sex_wide[complete.cases(sex_wide),c(-1,-2,-ncol(sex_wide))]

race <- as.data.frame(svytable(~RACE+CntyFIPS+ASTHNOW , des_current))
race_wide <- dcast(race, CntyFIPS+ASTHNOW   ~ RACE, value.var = "Freq")
race_wide$total <- rowSums(race_wide[,c(-1,-2)])
race_wide[,c(-1,-2)] <- race_wide[,c(-1,-2)]/race_wide$total
race_final <- race_wide[complete.cases(race_wide),c(-1,-2,-ncol(race_wide))]

bmi <- as.data.frame(svytable(~BMI+CntyFIPS+ASTHNOW , des_current))
bmi_wide <- dcast(bmi, CntyFIPS+ASTHNOW   ~ BMI, value.var = "Freq")
bmi_wide$total <- rowSums(bmi_wide[,c(-1,-2)])
bmi_wide[,c(-1,-2)] <- bmi_wide[,c(-1,-2)]/bmi_wide$total
bmi_final <- bmi_wide[complete.cases(bmi_wide),c(-1,-2,-ncol(bmi_wide))]

smoker <- as.data.frame(svytable(~SMOKER+CntyFIPS+ASTHNOW , des_current))
smoker_wide <- dcast(smoker, CntyFIPS+ASTHNOW   ~ SMOKER, value.var = "Freq")
smoker_wide$total <- rowSums(smoker_wide[,c(-1,-2)])
smoker_wide[,c(-1,-2)] <- smoker_wide[,c(-1,-2)]/smoker_wide$total
smoker_final <- smoker_wide[complete.cases(smoker_wide),c(-1,-2,-ncol(smoker_wide))]

educa <- as.data.frame(svytable(~EDUCA+CntyFIPS+ASTHNOW , des_current))
educa_wide <- dcast(educa, CntyFIPS+ASTHNOW   ~ EDUCA, value.var = "Freq")
educa_wide$total <- rowSums(educa_wide[,c(-1,-2)])
educa_wide[,c(-1,-2)] <- educa_wide[,c(-1,-2)]/educa_wide$total
educa_final <- educa_wide[complete.cases(educa_wide),c(-1,-2,-ncol(educa_wide))]

age_group <- as.data.frame(svytable(~AGE_GROUP+CntyFIPS+ASTHNOW , des_current))
age_group_wide <- dcast(age_group, CntyFIPS+ASTHNOW   ~ AGE_GROUP, value.var = "Freq")
age_group_wide$total <- rowSums(age_group_wide[,c(-1,-2)])
age_group_wide[,c(-1,-2)] <- age_group_wide[,c(-1,-2)]/age_group_wide$total
age_group_final <- age_group_wide[complete.cases(age_group_wide),c(-1,-2,-ncol(age_group_wide))]

all_years <- cbind(income_final, sex_final, race_final,
                   bmi_final, smoker_final, educa_final, age_group_final)
all_years$YEAR <- "all"
all <- cbind(all_years[,c("CntyFIPS", "YEAR", "ASTHNOW")], all_years[,3:27])

final_file <- rbind(yearly, all)

#write.csv(final_file, file = "weighted_current_variables_data.csv")
write.csv(final_file, file = "leaflet_app/data/weighted_current_variables.csv")

