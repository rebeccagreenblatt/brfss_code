library(survey)
library(dplyr)

asthnow <- read.csv("BRFSS_Dec_Edits/12.11.asthnow_final.csv")

table(asthma$ASTHMA, useNA = "ifany")
table(asthnow$ASTHNOW, useNA = "ifany")

asthnow$age_cat <- cut(asthnow$AGE, breaks = c(21, 34, 44, 54, 64, 100))

never_asthma <- filter(asthnow, ASTHNOW == 0)
never_asthma <- select(never_asthma, SEX, age_cat, RACE, EDUCA, INCOME, BMI, SMOKER)
current_asthma <- filter(asthnow, ASTHNOW == 1)
current_asthma <- select(current_asthma, SEX, age_cat, RACE, EDUCA, INCOME, BMI, SMOKER)

##current_asthma
current_counts <- unlist(lapply(current_asthma, table))
current_props <- unlist(lapply(current_asthma, function(col){prop.table(table(col))}))
current_combin <- paste0(formatC(current_counts, format="d",big.mark=","), ":", format(round(100*current_props, 2), nsmall=2))
names(current_combin) <- names(current_counts)
d <- current_combin
current_final <- rbind(t(t(d[c(2,1)])), "", t(t(d[c(12,9,11,8,10)])), "", 
                       t(t(d[c(14,15,13)])), "", t(t(d[c(17,18,16)])), "",
                       t(t(d[c(22,23,19,20,21)])), "", t(t(d[c(26,25,24)])), "", t(t(d[c(3:7)])))


##never_asthma
never_counts <- unlist(lapply(never_asthma, table))
never_props <- unlist(lapply(never_asthma, function(col){prop.table(table(col))}))
never_combin <- paste0(formatC(never_counts, format="d",big.mark=","), ":", format(round(100*never_props, 2), nsmall=2))
names(never_combin) <- names(never_counts)
d <- never_combin
never_final <- rbind(t(t(d[c(2,1)])), "", t(t(d[c(12,9,11,8,10)])), "", 
                     t(t(d[c(14,15,13)])), "", t(t(d[c(17,18,16)])), "",
                     t(t(d[c(22,23,19,20,21)])), "", t(t(d[c(26,25,24)])), "", t(t(d[c(3:7)])))

all <- cbind(current_final, never_final)

colnames(all) <- c("current", "never")

write.csv(all, file = "BRFSS_Dec_Edits/12.11.table2.csv")

##weighted column
library(survey)

never_asthma <- filter(asthnow, ASTHNOW == 0)
never_asthma <- select(never_asthma, SEX, age_cat, RACE, EDUCA, INCOME, BMI, SMOKER, CNTYWT, STSTR)
current_asthma <- filter(asthnow, ASTHNOW == 1)
current_asthma <- select(current_asthma, SEX, age_cat, RACE, EDUCA, INCOME, BMI, SMOKER, CNTYWT, STSTR)

never_asthma$asth <- "never"
current_asthma$asth <- "current"

all <- rbind(never_asthma, current_asthma)
all$asth.f <- factor(all$asth)

options(survey.lonely.psu = "adjust")
des <- svydesign(ids=~1, strata=~STSTR, weights=~as.numeric(CNTYWT),  data=all)

sex <- prop.table(svytable(~SEX+asth.f, des), 2)[c(2,1),]
race <- prop.table(svytable(~RACE+asth.f, des), 2)[c(5,2,4,1,3),]
educa <- prop.table(svytable(~EDUCA+asth.f, des), 2)[c(2,3,1),]
inc <- prop.table(svytable(~INCOME+asth.f, des), 2)[c(2,3,1),]
bmi <- prop.table(svytable(~BMI+asth.f, des), 2)[c(4,5,1,2,3),]
smoker <- prop.table(svytable(~SMOKER+asth.f, des), 2)[c(3,2,1),]
age <- prop.table(svytable(~age_cat+asth.f, des), 2)

sex <- format(round(100*sex, 1), nsmall=1)
race <- format(round(100*race, 1), nsmall=1)
educa <- format(round(100*educa, 1), nsmall=1)
inc <- format(round(100*inc, 1), nsmall=1)
bmi <- format(round(100*bmi, 1), nsmall=1)
smoker <- format(round(100*smoker, 1), nsmall=1)
age <- format(round(100*age, 1), nsmall=1)

r <- rep("", 2)
combin <- rbind(sex, r, race, r, educa, r, inc, r, bmi, r, smoker, r, age)

write.csv(combin, "BRFSS_Dec_Edits/12.11.weighted_table2_column.csv")




