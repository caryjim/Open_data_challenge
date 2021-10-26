#Utility script to aggregrate geographical information
crosswork <- read.csv("D:\\Program Files\\OneDrive - UNT System\\1_Digital Divide\\SEDA\\seda_crosswalk_4.0.csv", header = T)

head(crosswork)

colnames(crosswork)

geo_crosswk <- crosswork %>% filter(year == 2018) %>%
  select("fips", "stateabb", "sedacountyname", "sedametro", 
         "sedametroname", "countyid", "lzip", "lcity") # 73590 rows

sum(complete.cases(geo_crosswk)) #63849 

# keep only complete cases 
geo_crosswk <- na.omit(geo_crosswk)

getwd()

write.csv(geo_crosswk, "./Utiliy Scripts/seda-crosswk-simp.csv", row.names = F)

#only zip, county id, county name, state

geolayers <- select(geo_crosswk, c("lzip", "lcity", "countyid", "sedacountyname", "fips", "stateabb"))

write.csv(geolayers, "./Utiliy Scripts/zip-county-state.csv", row.names = F)

                    