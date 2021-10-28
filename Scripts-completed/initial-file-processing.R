#' Initial file processing for the open data challenge 

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

#' Broadband-Now Data has 32608 rows of zipcode level data and 23 variables
head(broadbandnow)

#Export the file for backup
#write.csv(broadbandnow, "./Datasets/broadbandnow-export.csv", row.names = F)

#-------------------Microsoft Broadband Usage------------------------------
#'The github dataset has been updated in June 2021. Therefore loading from original file in March 2021. 

# Broadband Usage Estimates by County Level in Nov 2019
usage_county <- read.csv("./Datasets/Microsoft/broadband_data-2019-v1.csv", 
                         header = T, sep = ",", strip.white = TRUE, fileEncoding="UTF-8-BOM")

# Broadband Usage Estimates by Zip Code 
usage_zip <- read.csv("./Datasets/Microsoft/broadband_data_zipcode-2019-v1.csv",
                      header = T, sep = ",", strip.white = TRUE)

# There are 3143 rows of county in the dataset with 5 variables 
head(usage_county)

# There are 32735 rows of postal code -ZIP- in the dataset with 8 variables
head(usage_zip)

# Joint microsoft dataset
usage_combined <- left_join(usage_zip, usage_county, by = "COUNTY.ID")
# There are 32725 rows in this joint dataframe 
head(usage_combined)

# Rename column heading in order to match 
colnames(usage_combined)[4] <- "Zip"
head(usage_combined)

# Joint with Broadband-Now dataframe
combined <- left_join(usage_combined, broadbandnow, by = "Zip")
# There are 32735 rows with 34 variables in this dataframe 
head(combined)
# Columns Name, which shows there are 2015 values in the broadband-now data 
colnames(combined) 
# Data types of the combined broadband datasets
str(combined)

# Convert COUNTY.ID to characters before joining with other files
combined[3] <- as.character(combined$COUNTY.ID)

# What is not combined between the two? 
leftover <- anti_join(usage_combined, broadbandnow, by = "Zip")
# The leftover dataframe shows 3250 rows of postal code that was not joined.
write.csv(leftover,"D:/Documents/R/Open_data_challenge/unmatched-records-broadband.csv")

#------------Census Tables--------------------------------------------------------------
# Part 2 Cenus ACS data of households with children (under 18 years old) having computer/access to internet 
# Census B28005 Table

b28005 <- read_excel('./Datasets/b28005-5yr estimates-geo.xlsx')
# There are 3220 county listed in the Census Bureau ACS table 
head(b28005)
str(b28005)

# Append the Census Data to the combined dataset 
combined_2 <- left_join(combined, b28005, by = "COUNTY.ID")

# View datatypes after joining
str(combined_2)

#' There is an older dataset for rurality from Census 2010 
rurality <- read_excel('./Datasets/County_Rural_Level_2010.xlsx')
head(rurality)

# County.ID lost the leading zero when load in rurality 
rurality[1] <- as.character(rurality$COUNTY.ID)

# The last column naming is problematic
names(rurality)[5] <- "rurality"

# There are 3142 county listed in the rurality dataset
str(rurality)

# Joint of broadband data, b28005 table, and rurality measure
combined_3 <-left_join(combined_2, rurality, by = "COUNTY.ID")

# There are 32735 rows with 48 variables
str(combined_3)

# Cleaned up duplicated information in this dataframe to 20 variables 
census_broadband <- select(combined_3, c("COUNTY.ID", "Zip", "BROADBAND.USAGE.x",
                                      "BROADBAND.USAGE.y", "Population", "AverageMbps", "FastestAverageMbps",
                                      "TestCount", "X.Access.to.Terrestrial.Broadband", "Total",
                                      "Student", "Has-computer", "Has-computer-and-broadband",
                                      "Has-computer-and-nointernet", "No-computer",
                                      "Dial-up", "ST", "County", "State", "rurality"))

#---------------------NCES Public School Data-----------------------

# I found an issue with the ELIS data where schools are listed but is not on EDGE list. 
# So I have to recheck the data in this step (March 9, 2021)

public_school <-read_excel('./Datasets/public_schoolsv2.xlsx')

#public school file exported from ELIS, when importing the excel file it was problematic with mixed data types.
#Convert the excel file to csv for error free import (Note that csv version is 21 MB, instead of 11 MB in excel format)

colnames(public_school)

head(public_school)
str(public_school)

get_dupes(public_school)

public_school$Zip <- as.numeric(public_school$Zip)

#Combine the school data with the broadband data 
sch_broadband <- left_join(public_school, census_broadband, by = "Zip")

#write.csv(sch_broadband, "./Datasets/sch_broadband_combov2.csv", row.names = F)

#Load the title 1 status from the ELSI file 
title1 <- read.csv("./Datasets/ELSI_csv_export-m.csv", header = T, 
                   sep = ",", strip.white = TRUE, fileEncoding="UTF-8-BOM")

# data <- read_excel(file.choose())  

all_schools <- read.csv("./Datasets/Public_School_Characteristics_2018-19.csv", header = T,
                        sep = ",", strip.white = TRUE, fileEncoding="UTF-8-BOM")

# SY_STATUS_TEXT indicates if the school is currently operational 
all_schools <- subset(all_schools, SY_STATUS_TEXT == "Currently operational")

#------------Factor score --------------------
#Final dataset that generated factor scores in SPSS 
factor_score <-read.csv("./Datasets/factor_score.csv", header = T, fileEncoding="UTF-8-BOM")
str(factor_score)
