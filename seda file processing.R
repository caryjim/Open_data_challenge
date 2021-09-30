#Part 4 SEDA data 
#SEDA data of test scores and covarites by county level 

testscore <- read.csv("./SEDA/seda_county_long_cs_4.0.csv", header = T, sep = ",",
                      strip.white = TRUE)

covariates <-read.csv("./SEDA/seda_cov_county_long_4.0.csv", header = T, sep = ",",
                      strip.white = TRUE)

#First, have to filter out only the 2018 dataset
score2018 <-testscore%>% filter(year == "2018")
covariates2018 <- covariates%>% filter(year == 2018)

#State info is missing from covariates file
fips<- read.csv("fips_state.txt", sep="\t")

score_m <-score2018[, c(1:6, 8:10)]
covariates_m <-left_join(covariates2018, fips, by = "fips")

# Rename sedacounty to COUNTY.ID
colnames(covariates_m)[1] <- "COUNTY.ID"
colnames(score_m)[3] <- "COUNTY.ID"


# Export covariates_m, score_m, school_status, combined_3, broadband_seda
write.csv(covariates_m,"D:/Documents/R/Digital Divide/Open_Data_Challenge/seda_covariates.csv")
write.csv(score_m,"D:/Documents/R/Digital Divide/Open_Data_Challenge/seda_scores.csv")
write.csv(combined_3,"D:/Documents/R/Digital Divide/Open_Data_Challenge/broadband_agg.csv")
write.csv(broadband_seda,"D:/Documents/R/Digital Divide/Open_Data_Challenge/broadband_seda.csv")

# Import a difference covariate file 
covariates_2 <-read.csv("./SEDA/seda_cov_county_poolyr_4.0.csv", header = T, sep = ",",
                      strip.white = TRUE)
library(tidyverse)
covariates2017 <-covariates_2%>% filter(year == 2017)
covariates2018 <- covariates_2%>% filter(year == 2018)
colnames(covariates2018)[1] <- "COUNTY.ID"
colnames(covariates2018)
colnames(covariates2017)[1] <- "COUNTY.ID"
colnames(covariates2017)
seda_covariate <-covariates2018[,c(1,2, 9:16, 27)]
write.csv(seda_covariate,"D:/Documents/R/Digital Divide/Open_Data_Challenge/Analysis/seda_covariates.csv", row.names = F)
#to extract values needs for the missing one in 2018. 
seda_covariate17 <-covariates2017[,c(1,2, 9:16, 27)]

#Re-load files into this space
seda_cov <- read.csv("seda_covariates.csv", header = T)

factor_score <-read.csv("./SPSS/factor_score.csv", header = T, fileEncoding="UTF-8-BOM")
library(tidyverse)
library(dplyr)
#unmatched <- anti_join(factor_score, seda_cov, by = "COUNTY.ID")

unmatched <-anti_join(covariates2017, covariates2018, by = "COUNTY.ID")
write.csv(unmatched,"D:/Documents/R/Digital Divide/Open_Data_Challenge/Analysis/unmatched.csv", row.names = F)
