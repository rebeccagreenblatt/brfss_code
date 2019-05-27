library(dplyr)

complete_raw <- read.csv("BRFSS_Dec_Edits/12.11.asthnow_final.csv")
missing_raw <- read.csv("BRFSS_Dec_Edits/12.11.asthnow_missing.csv")

complete_raw$group <- "complete"
missing_raw$group <- "missing"

complete_raw$age_cat <- cut(complete_raw$AGE, breaks = c(21, 34, 44, 54, 64, 100))
missing_raw$age_cat <- cut(missing_raw$AGE, breaks = c(21, 34, 44, 54, 64, 100))

complete <- select(complete_raw, ASTHNOW, SEX, age_cat, RACE, EDUCA, INCOME, BMI, SMOKER)
missing <- select(missing_raw, ASTHNOW, SEX, age_cat, RACE, EDUCA, INCOME, BMI, SMOKER)

##complete
complete_counts <- unlist(lapply(complete, table))
complete_props <- 100*unlist(lapply(complete, function(col){prop.table(table(col))}))
complete_combin <- paste0(formatC(complete_counts, format="d", big.mark=","), " (", format(round(complete_props, 2), nsmall = 2), ")")
names(complete_combin) <- names(complete_counts)
d <- complete_combin
complete_final <- rbind("", t(t(d[c(1,2)])), "", t(t(d[c(4,3)])), "", t(t(d[c(14,11,13,10,12)])), "", 
                        t(t(d[c(16,17,15)])), "", t(t(d[c(19,20,18)])), "",
                        t(t(d[c(24,25,21,22,23)])), "", t(t(d[c(28,27,26)])), "", t(t(d[c(5:9)])))


##missing
missing_counts <- unlist(lapply(missing, table))
missing_props <- 100*unlist(lapply(missing, function(col){prop.table(table(col))}))
missing_combin <- paste0(formatC(missing_counts, format="d", big.mark=","), " (", format(round(missing_props, 2), nsmall = 2), ")")
names(missing_combin) <- names(missing_counts)
d <- missing_combin
missing_final <- rbind("", t(t(d[c(1,2)])), "", t(t(d[c(4,3)])), "", t(t(d[c(14,11,13,10,12)])), "", 
                       t(t(d[c(16,17,15)])), "", t(t(d[c(19,20,18)])), "",
                       t(t(d[c(24,25,21,22,23)])), "", t(t(d[c(28,27,26)])), "", t(t(d[c(5:9)])))

all_asthma <- rbind(complete, missing)

asthma_miss <- cbind(formatC(table(is.na(all_asthma$ASTHNOW))[2], format="d", big.mark=","), format(round(prop.table(table(is.na(all_asthma$ASTHNOW)))[2], 4)*100, nsmall = 2))
sex_miss <- cbind(formatC(table(is.na(all_asthma$SEX))[2], format="d", big.mark=","), format(round(prop.table(table(is.na(all_asthma$SEX)))[2], 4)*100, nsmall = 2))
race_miss <- cbind(formatC(table(is.na(all_asthma$RACE))[2], format="d", big.mark=","), format(round(prop.table(table(is.na(all_asthma$RACE)))[2], 4)*100, nsmall = 2))
educa_miss <- cbind(formatC(table(is.na(all_asthma$EDUCA))[2], format="d", big.mark=","), format(round(prop.table(table(is.na(all_asthma$EDUCA)))[2], 4)*100, nsmall = 2))
income_miss <- cbind(formatC(table(is.na(all_asthma$INCOME))[2], format="d", big.mark=","), format(round(prop.table(table(is.na(all_asthma$INCOME)))[2], 4)*100, nsmall = 2))
smoker_miss <- cbind(formatC(table(is.na(all_asthma$SMOKER))[2], format="d", big.mark=","), format(round(prop.table(table(is.na(all_asthma$SMOKER)))[2], 4)*100, nsmall = 2))
bmi_miss <- cbind(formatC(table(is.na(all_asthma$BMI))[2], format="d", big.mark=","), format(round(prop.table(table(is.na(all_asthma$BMI)))[2], 4)*100, nsmall = 2))
age_miss <- cbind(formatC(table(is.na(all_asthma$age_cat))[2], format="d", big.mark=","), format(round(prop.table(table(is.na(all_asthma$age_cat)))[2], 4)*100, nsmall = 2))

unweighted_missing <- rbind(asthma_miss, "", "", sex_miss, "", "", race_miss, "", "", "", "", "", educa_miss, "", "", "", income_miss, "", "", "", bmi_miss, "", "", "", "", "", smoker_miss, "", "", "", age_miss, "", "", "","", "")
names <- c("asthma", "", "", "sex", "", "", "race", "", "", "", "", "", "educa", "", "", "", "income", "", "", "", "bmi", "", "", "", "", "", "smoker", "", "", "", "age", "", "", "","", "")
rownames(unweighted_missing) <- names
unweighted_missing[4,] <- c("0", "0.00")
combin <- ifelse(unweighted_missing[,1] >= 0, paste0(unweighted_missing[,1], " (", unweighted_missing[,2], ")"), "")
names(combin) <- names

all <- cbind(combin, complete_final, missing_final)

write.csv(all, file = "BRFSS_Dec_Edits/12.11.table1.csv")



