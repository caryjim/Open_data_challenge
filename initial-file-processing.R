#' Intial file processing for the open data challenge 

library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(janitor)
library(psych)
library(readxl)
library(psychTools)

#' Broadband-Now Released Data on Github
broadbandurl <- 'https://raw.githubusercontent.com/BroadbandNow/Open-Data/master/broadband_data_opendatachallenge.csv'
broadbandnow <-read.csv(url(broadbandurl), header = T, sep = ",", strip.white = TRUE)

head(broadband)

# Broadband Usage Estimates by County Level 
microsoft <- 'https://raw.githubusercontent.com/microsoft/USBroadbandUsagePercentages/3cc660f1f54fd73274527a4a60ef870726abf2ba/dataset/broadband_data.csv'

usage_county <- read.csv(url(microsoft), header = T, sep = ",", strip.white = TRUE, fileEncoding="UTF-8-BOM")

# Broadband Usage Estimates by Zip Code 
microsoft_by_zip <- 'https://raw.githubusercontent.com/microsoft/USBroadbandUsagePercentages/3cc660f1f54fd73274527a4a60ef870726abf2ba/dataset/broadband_data_zipcode.csv'

usage_zip <- read.csv(url(microsoft_by_zip), header = T, sep = ",", strip.white = TRUE)

head(usage_county)

head(usage_zip)

## Can we append the two microsoft dataset and then join the broadband data?
usage_combined <- left_join(usage_zip, usage_county, by = "COUNTY.ID")
head(usage_combined)

#Rename column heading in order to match 
colnames(usage_combined)[4] <- "Zip"
head(usage_combined)

combined <- left_join(usage_combined, broadband, by = "Zip")
head(combined)
colnames(combined) 

#What is not combined?
leftover <- anti_join(usage_combined, broadband, by = "Zip")
write.csv(leftover,"D:/Documents/R/Digital Divide/Open_Data_Challenge/leftover.csv")

# Part 2 Cenus data of households with children (under 18 years old) having computer/access to internet 
## Census B28005 Table reformatted version with GEOid recode as COUNTY.ID

b28005 <- read_excel('b28005-5yr estimates-geo.xlsx')
head(b28005)

#COUNTY.ID is stored as character in the Census original file 
#Convert it to integer for now in order to map to the combined dataset 
b28005$COUNTY.ID <- as.integer(b28005$COUNTY.ID)
#Note that you lost the leading zero when convert county.id to integer

#Append the Census Data to the combined dataset 
combined_2 <- left_join(combined, b28005, by = "COUNTY.ID")

rurality <- read_excel('County_Rural_Level_2010.xlsx')
head(rurality)
#County.ID lost the leading zero when load in rurality 

combined_3 <-left_join(combined_2, rurality, by = "COUNTY.ID")

#-----------------------------------------------------------------------

#Part 3 School Data
#I found an issue with the ELIS data where schools are listed but is not on EDGE list. 
#So I have to recheck the data in this step (March 9, 2021)
#Append school location and school characteristics first before merging 

#data <- read_excel(file.choose())  

#schools <- read.csv("Public_School_Characteristics_2018-19.csv", header = T, 
#                    sep = ",", strip.white = TRUE, fileEncoding="UTF-8-BOM")

#SY_STATUS_TEXT indicates if the school is currently operational 
#schools <- subset(schools, SY_STATUS_TEXT == "Currently operational")

#schools_loc2 <- left_join(schools, school_loc, by = "NCESSCH")
#school_f <- schools_loc2[, c(4, 14:15, 40, 47, 48:51, 69:70, 90)]
#write.csv(schools_loc2, "D:/Documents/R/Digital Divide/Open_Data_Challenge/error_file.csv" )

#School location information
#public school file exported from ELIS
public_school <-read.csv('public_schoolsv2.csv', na = "na", header = T,
                         strip.white = T, stringsAsFactors = TRUE, fileEncoding="UTF-8-BOM")

#public school file exported from ELIS, when importing the excel file it was problematic with mixed data types.
#Convert the excel file to csv for error free import (Note that csv version is 21 MB, instead of 11 MB in excel format)

colnames(public_school)

head(public_school)
str(public_school)

get_dupes(public_school)

#Combine the school data with the broadband data 
sch_broadband_combined <- left_join(public_school, combined_3, by= "Zip")

write.csv(sch_broadband_combined,"D:/Documents/R/Digital Divide/Open_Data_Challenge/Export/sch_broadband_combov2.csv")


#Rename County ID column and Zip for merge later 
#colnames(school_f)[3] <- "Zip"
#colnames(school_f)[12] <- "COUNTY.ID"
#colnames(school_f)

#Load the title 1 status from the ELSI file 
#title1 <- read.csv("ELSI_csv_export-m.csv", header = T, sep = ",", strip.white = TRUE, fileEncoding="UTF-8-BOM")

#Merging the title1 and school_f file 
#sch_status <- left_join(school_f, title1, by = "NCESSCH")

#-------------------------------------------------------------
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

#Append the broadband data to the school data by zip code 
#school_profile is the cleaned version
#broadband_combined is the cleaned version
sch <- read.csv("school_profile.csv")
broad_cleaned <-read.csv("broadband_combined.csv")

school_broadband_combo <-left_join(sch, broad_cleaned, by = "Zip")
write.csv(school_broadband_combo, "D:/Documents/R/Digital Divide/Open_Data_Challenge/school_broadband_combined.csv" )
