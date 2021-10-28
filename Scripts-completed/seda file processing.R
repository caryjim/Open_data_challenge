library(tidyverse)

#Part 4 SEDA data processing 
#SEDA data of test scores and covarites by county level 

testscore <- read.csv("./Datasets/SEDA/seda_county_long_cs_4.0.csv", header = T, sep = ",",
                      strip.white = TRUE) #338406 observations

covariates <-read.csv("./Datasets/SEDA/seda_cov_county_long_4.0.csv", header = T, sep = ",",
                      strip.white = TRUE) #191197 observations

# Subset only the 2018 dataset from SEDA 4.0
score2018 <-testscore%>% filter(year == 2018)
covariates2018 <- covariates %>% filter(year == 2018)
covariates2017 <- covariates %>% filter(year == 2017)

# View dataframe and data types of test scores 
head(score2018) # 33027 observations 
colnames(score2018) # We only need the first 10 columns, no need for score by race

score2018 <- select(score2018, c(1:10))

str(score2018)  
# Rename the sedacounty column to COUNTY.ID
colnames(score2018)[3] <- "COUNTY.ID"

# Convert COUNTY.ID to character 
score2018[3] <- as.character(score2018$COUNTY.ID)

# Count unique county in score2018 
score2018 %>% group_by(COUNTY.ID) %>% summarise(count = n_distinct(COUNTY.ID))
# There are 3055 COUNTY.ID distinct count 

#write.csv(score2018, "./Datasets/SEDA/score2018.csv", row.names = F)

#--- SEDA covariates----------------------------------------------

head(covariates2018) #19014 observations
colnames(covariates2018)

# Subset the needed columns 
cov_2018 <- select(covariates2018, c(1:18, 27:28))
str(cov_2018) 

# Rename sedacounty to COUNTY.ID
colnames(cov_2018)[1] <- "COUNTY.ID"
cov_2018[1] <- as.character(cov_2018$COUNTY.ID)

#write.csv(cov_2018, "./Datasets/SEDA/covariates-2018.csv", row.names = F)

head(covariates2017) #19250 observations
colnames(covariates2017)

# Subset the needed columns 
cov_2017 <- select(covariates2017, c(1:18, 27:28))
str(cov_2017) 

# Rename sedacounty to COUNTY.ID
colnames(cov_2017)[1] <- "COUNTY.ID"
cov_2017[1] <- as.character(cov_2017$COUNTY.ID)

#write.csv(cov_2017, "./Datasets/SEDA/covariates-2017.csv", row.names = F)

# Note. The 2018 dataset is missing information for several counties. 
# Therefore, I had to use the 2017 school year from the covariate dataset for
# SES values for several regions. 

unjoined_cov <- anti_join(cov_2017, cov_2018, by = c("COUNTY.ID", "grade")) # Check what is not joined.

str(unjoined_cov)

# There are 236 unjoined rows which included 6 level of grades, which is 38 counties

cov_combined <- left_join(cov_2017, cov_2018, by = c("COUNTY.ID", "grade"))
# There are 19250 observation returned

head(cov_combined)

#write.csv(cov_combined, "./Datasets/SEDA/COV-combined-2017to2018.csv", row.names = F)
