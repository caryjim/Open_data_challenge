## Dallas County as an example
# I'm assessing how does the does the data looks like for a metropolitan area 

```{r}
dallas <- broadbandnow %>% filter(County == 'Dallas')
# this return all rows with Dallas county regardless of which state
dallas
```
```{r}
dallas <-broadbandnow %>% filter(State == 'Texas') %>% filter (County == 'Dallas')
#This filter by state and then by county to get only Dallas County data within Texas

dallas
```

```{r}
# As you noticed, there are quiet a few of missing information per zip code within Dallas County in Texas
summary(dallas)

```
# 38 rows missing within 122 rows is about 31%

# After checking the postal codes in [City-Data.com](https://www.city-data.com/zipDir.html), for example 75391-75399, this set of zip code doesn't have any population information associated with it. 

## Using a rural area to check the dataset. The census data has a [Rural America](https://mtgis-portal.geo.census.gov/arcgis/apps/MapSeries/index.html?appid=49cd4bc9c8eb444ab51218c1d5001ef6#:~:text=The%20Census%20Bureau%20defines%20rural,tied%20to%20the%20urban%20definition) page for county rurality. 

```{r}
# Crockett County, Texas with a 22.7 Percent Rural 
crockett <- broadbandnow %>% filter(County == 'Crockett')
crockett
#There are five rows filter and with only one row for Texas 
```

```{r}
# Schleicher County, Texas with a 100 Percent Rural 
schleicher <- broadbandnow %>% filter(County == 'Schleicher')
schleicher
#There is only one result for this Schleicher in the dataset 
```

## We would like to add a new column to the 2019 dataset considering the real price that a user will pay for internet including taxes and fees. Currently we estimated a 25% on top of the internet charge as taxes and fees

```{r}
# Append a column to the dataset with consideration of fees and tax on final price 
data2019$est_final_price <- broadbandnow$Month_price * 0.25
```

```



