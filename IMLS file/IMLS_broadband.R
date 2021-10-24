library(tidyverse)
library(readxl)
library(psych)
setwd("D:/Documents/R/Digital Divide/Open_Data_Challenge/IMLS")

imls <- read_excel('publicdata_imls_metrics.xlsx', sheet = 1)
imls_state <-read_excel('publicdata_imls_metrics.xlsx', sheet = 2)
colnames(imls)
str(imls)

colnames(imls_state)
str(imls_state)

#imls$`Population for whom broadband available, 2019 (%)` <-as.numeric(imls$`Population for whom broadband available, 2019 (%)`)

#plot(imls$State,imls$`Population for whom broadband available, 2019 (%)`, main="Population of wholm broadband is available (2019)",
#     xlab="State") 

par(mar = c(8,6,4,1)+ .1)
barplot(height = imls_state$`Population for whom broadband available, 2019 (%)`,
        names = imls_state$State,
        space = 0.5,
        ylab = "Percentage",
        main = "Percent of Broadband Availability by Population per State (2019)",
        ylim = c(0, 100),
        las = 2,
        cex.names = 0.8)


hist(imls$`Population for whom broadband available, 2019 (%)`)

plot(imls$`Population for whom broadband available, 2019 (%)`, main="Percent of broadband available",
     xlab="Broadband Availability ", ylab="Frequency", pch=19) 


boxpimls <- read_excel('publicdata_imls_metrics.xlsx', sheet = 1)
colnames(imls)
str(imls)

pimls <- read_excel('publicdata_imls_metrics.xlsx', sheet = 1)
colnames(imls)
str(imls)lot(imls$'Population for whom broadband available, 2019 (%)', horizontal = TRUE, ylab = "Avg_Month_Price") #las turn labels to vertical 

