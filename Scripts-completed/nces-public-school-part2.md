NCES Public School 2018-2019 Data Processing
================
Cary K. Jim

Library/Package

``` r
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 4.0.5

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.3     v dplyr   1.0.7
    ## v tidyr   1.1.3     v stringr 1.4.0
    ## v readr   2.0.0     v forcats 0.5.1

    ## Warning: package 'ggplot2' was built under R version 4.0.5

    ## Warning: package 'readr' was built under R version 4.0.5

    ## Warning: package 'dplyr' was built under R version 4.0.5

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(janitor)
```

    ## 
    ## Attaching package: 'janitor'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     chisq.test, fisher.test

Data Source: U.S. Department of Education, National Center for Education
Statistics, Common Core of Data (CCD), “Public Elementary/Secondary
School Universe Survey”, 2018-19; “Public Elementary/Secondary School
Universe Survey Geographic Data (EDGE)”, 2018-2019.

† indicates that the data are not applicable. – indicates that the data
are missing. ‡ indicates that the data do not meet NCES data quality
standards.

``` r
public_school <-read.csv('./Datasets/public_schoolv3.csv', na.strings = c("","-","‡"), 
                         header = T, strip.white = T, sep = ",",
                         fileEncoding="UTF-8-BOM")
```

Part 1 of exploring data can be found in RPubs page
<https://rpubs.com/caryjim/nces-ps-status>

# Part 2. Data Cleaning and Processing

## Task 2.1 Remove schools that is not in operation or with no enrollment

``` r
# Remove schools that have no enrollment information, the subset function will also drop rows with no values
public_school <- subset(public_school, !(Members == "0"))  #97225 rows
```

``` r
#Remove schools that have no enrollment information, n/a is not applicable 
public_school <- subset(public_school, !(Members == "n/a")) #95267 rows
```

``` r
#Check how the school status looks like after removing schools with no enrollment information
#Do we have any information for school that is "Added" or "Future" status? 
table(public_school$Updated.Status)
```

    ## 
    ##                    1-Open                     3-New                   4-Added 
    ##                     94139                       960                        33 
    ## 5-Changed Boundary/Agency                8-Reopened 
    ##                       105                        30

## Task 2.2 Remove schools based on Agency Type

``` r
# We will exclude Regional Education Service Agency (RESA)
public_school <- subset(public_school, !(Agency.Type == "4-Regional Education Service Agency (RESA)")) #94619 rows
```

``` r
table(public_school$Agency.Type)
```

    ## 
    ## 1-Regular local school district that is NOT a component of a supervisory union 
    ##                                                                          87451 
    ##             2-Local school district that is a component of a supervisory union 
    ##                                                                           2444 
    ##         5-State agency providing elementary and/or secondary level instruction 
    ##                                                                            476 
    ##                                                 7-Independent Charter District 
    ##                                                                           4105 
    ##                                                     8-Other education agencies 
    ##                                                                             30 
    ##                                           9-Specialized public school district 
    ##                                                                            113

We don’t want to simply drop a school if it is not a regular school.
Therefore, it is best to include specialized schools that are serving
students who may not be enrolled in a general education classroom.

\#\#\#Task 2.3 Review the incomplete cases at this point

``` r
incomplete_sch <- public_school[!complete.cases(public_school),] #3523 rows
#It looks like FTE is the column that is causing the incomplete cases at this time.
head(incomplete_sch)
```

    ##     State.Name ST                      School.Name  NCES.SchID
    ## 3    Minnesota MN        112 ALC Independent Study 2.70819e+11
    ## 4    Minnesota MN            112 ALC MIDDLE SCHOOL 2.70819e+11
    ## 108   Illinois IL          A J Katzenmaier Academy 1.70011e+11
    ## 129   Illinois IL         A O Marshall Elem School 1.72058e+11
    ## 163  Louisiana LA A. E. Phillips Laboratory School 2.20006e+11
    ## 209    Arizona AZ           AAEC - Paradise Valley 4.00106e+10
    ##                                     Agency_Name     County_Name COUNTY.ID
    ## 3           EASTERN CARVER COUNTY PUBLIC SCHOOL   Carver County     27019
    ## 4           EASTERN CARVER COUNTY PUBLIC SCHOOL   Carver County     27019
    ## 108                        North Chicago SD 187     Lake County     17097
    ## 129                               Joliet PSD 86     Will County     17197
    ## 163             A.E. Phillips Laboratory School  Lincoln Parish     22061
    ## 209 Arizona Agribusiness & Equine Center Inc. 1 Maricopa County      4013
    ##     Urban.centric.Locale Latitude  Longitude
    ## 3       21-Suburb: Large 44.84528  -93.57477
    ## 4       21-Suburb: Large 44.84161  -93.59692
    ## 108     21-Suburb: Large 42.32605  -87.86273
    ## 129     21-Suburb: Large 41.53138  -88.04488
    ## 163     32-Town: Distant 32.52588  -92.65047
    ## 209       11-City: Large 33.64847 -112.01150
    ##                    Title.I.School.Status Updated.Status
    ## 3   2-Title I targeted assistance school         1-Open
    ## 4   2-Title I targeted assistance school         1-Open
    ## 108                                 <NA>         1-Open
    ## 129                                 <NA>         1-Open
    ## 163                                 <NA>         1-Open
    ## 209               6-Not a Title I school         1-Open
    ##                                                                        Agency.Type
    ## 3   1-Regular local school district that is NOT a component of a supervisory union
    ## 4   1-Regular local school district that is NOT a component of a supervisory union
    ## 108 1-Regular local school district that is NOT a component of a supervisory union
    ## 129 1-Regular local school district that is NOT a component of a supervisory union
    ## 163                                                     8-Other education agencies
    ## 209                                                 7-Independent Charter District
    ##                    School.Type Members Male Female FTE.Equivalent
    ## 3   4-Alternative/other school      37   20     17           <NA>
    ## 4   4-Alternative/other school     366  198    168           <NA>
    ## 108           1-Regular school     436  228    208             28
    ## 129           1-Regular school     485  249    236             29
    ## 163           1-Regular school     397  201    196             22
    ## 209           1-Regular school     313  107    206           <NA>
    ##     Pupil.Teacher.Ratio
    ## 3                   n/a
    ## 4                   n/a
    ## 108               15.57
    ## 129               16.72
    ## 163               18.05
    ## 209                 n/a

There are still a subset of schools that contain missing values, such as
“Title.I. School.Status”, “FTE.Equivalent”, and “Pupil.Teacher.Ratio”.
It won’t be significantly affect the following steps. So we will leave
these missing values in the data for now.

## Task 3. Convert Data Types

``` r
str(public_school)
```

    ## 'data.frame':    94619 obs. of  19 variables:
    ##  $ State.Name           : chr  "Massachusetts" "Washington" "Minnesota" "Minnesota" ...
    ##  $ ST                   : chr  "MA" "WA" "MN" "MN" ...
    ##  $ School.Name          : chr  "1 LT Charles W. Whitcomb School" "10th Street School" "112 ALC Independent Study" "112 ALC MIDDLE SCHOOL" ...
    ##  $ NCES.SchID           : num  2.51e+11 5.30e+11 2.71e+11 2.71e+11 2.63e+11 ...
    ##  $ Agency_Name          : chr  "Marlborough" "Marysville School District" "EASTERN CARVER COUNTY PUBLIC SCHOOL" "EASTERN CARVER COUNTY PUBLIC SCHOOL" ...
    ##  $ County_Name          : chr  "Middlesex County" "Snohomish County" "Carver County" "Carver County" ...
    ##  $ COUNTY.ID            : int  25017 53061 27019 27019 26077 31079 18089 42029 48215 55025 ...
    ##  $ Urban.centric.Locale : chr  "21-Suburb: Large" "22-Suburb: Mid-size" "21-Suburb: Large" "21-Suburb: Large" ...
    ##  $ Latitude             : num  42.4 48.1 44.8 44.8 42.2 ...
    ##  $ Longitude            : num  -71.5 -122.2 -93.6 -93.6 -85.6 ...
    ##  $ Title.I.School.Status: chr  "5-Title I schoolwide school" "6-Not a Title I school" "2-Title I targeted assistance school" "2-Title I targeted assistance school" ...
    ##  $ Updated.Status       : chr  "1-Open" "1-Open" "1-Open" "1-Open" ...
    ##  $ Agency.Type          : chr  "1-Regular local school district that is NOT a component of a supervisory union" "1-Regular local school district that is NOT a component of a supervisory union" "1-Regular local school district that is NOT a component of a supervisory union" "1-Regular local school district that is NOT a component of a supervisory union" ...
    ##  $ School.Type          : chr  "1-Regular school" "1-Regular school" "4-Alternative/other school" "4-Alternative/other school" ...
    ##  $ Members              : chr  "1308" "178" "37" "366" ...
    ##  $ Male                 : chr  "681" "77" "20" "198" ...
    ##  $ Female               : chr  "627" "101" "17" "168" ...
    ##  $ FTE.Equivalent       : chr  "118.8" "7.3" NA NA ...
    ##  $ Pupil.Teacher.Ratio  : chr  "11.01" "24.38" "n/a" "n/a" ...

## Task 3.1 Enrollment Information

Note: We have to pass the value as characters first, otherwise R will
assign a level value

``` r
#Members, Male, Female, FTE and Pupil.Teacher Ratio are as a factor level due to the symbol used in the original data 
public_school$Members <- as.integer(as.character(public_school[,15]))
```

``` r
head(public_school)
```

    ##      State.Name ST                     School.Name  NCES.SchID
    ## 1 Massachusetts MA 1 LT Charles W. Whitcomb School 2.50732e+11
    ## 2    Washington WA              10th Street School 5.30486e+11
    ## 3     Minnesota MN       112 ALC Independent Study 2.70819e+11
    ## 4     Minnesota MN           112 ALC MIDDLE SCHOOL 2.70819e+11
    ## 6      Michigan MI          12th Street Elementary 2.62895e+11
    ## 8      Nebraska NE                   1R ELEMENTARY 3.17458e+11
    ##                           Agency_Name      County_Name COUNTY.ID
    ## 1                         Marlborough Middlesex County     25017
    ## 2          Marysville School District Snohomish County     53061
    ## 3 EASTERN CARVER COUNTY PUBLIC SCHOOL    Carver County     27019
    ## 4 EASTERN CARVER COUNTY PUBLIC SCHOOL    Carver County     27019
    ## 6              Portage Public Schools Kalamazoo County     26077
    ## 8            NORTHWEST PUBLIC SCHOOLS      Hall County     31079
    ##   Urban.centric.Locale Latitude  Longitude                Title.I.School.Status
    ## 1     21-Suburb: Large 42.35735  -71.54859          5-Title I schoolwide school
    ## 2  22-Suburb: Mid-size 48.05744 -122.18199               6-Not a Title I school
    ## 3     21-Suburb: Large 44.84528  -93.57477 2-Title I targeted assistance school
    ## 4     21-Suburb: Large 44.84161  -93.59692 2-Title I targeted assistance school
    ## 6       13-City: Small 42.22038  -85.64661               6-Not a Title I school
    ## 8     41-Rural: Fringe 41.00278  -98.37956               6-Not a Title I school
    ##   Updated.Status
    ## 1         1-Open
    ## 2         1-Open
    ## 3         1-Open
    ## 4         1-Open
    ## 6         1-Open
    ## 8         1-Open
    ##                                                                      Agency.Type
    ## 1 1-Regular local school district that is NOT a component of a supervisory union
    ## 2 1-Regular local school district that is NOT a component of a supervisory union
    ## 3 1-Regular local school district that is NOT a component of a supervisory union
    ## 4 1-Regular local school district that is NOT a component of a supervisory union
    ## 6 1-Regular local school district that is NOT a component of a supervisory union
    ## 8 1-Regular local school district that is NOT a component of a supervisory union
    ##                  School.Type Members Male Female FTE.Equivalent
    ## 1           1-Regular school    1308  681    627          118.8
    ## 2           1-Regular school     178   77    101            7.3
    ## 3 4-Alternative/other school      37   20     17           <NA>
    ## 4 4-Alternative/other school     366  198    168           <NA>
    ## 6           1-Regular school     530  269    261          29.26
    ## 8           1-Regular school     182   96     86          12.65
    ##   Pupil.Teacher.Ratio
    ## 1               11.01
    ## 2               24.38
    ## 3                 n/a
    ## 4                 n/a
    ## 6               18.11
    ## 8               14.39

``` r
public_school$Male <- as.integer(as.character(public_school[,16]))
```

    ## Warning: NAs introduced by coercion

``` r
public_school$Female <- as.integer(as.character(public_school[,17]))
```

    ## Warning: NAs introduced by coercion

``` r
head(public_school)
```

    ##      State.Name ST                     School.Name  NCES.SchID
    ## 1 Massachusetts MA 1 LT Charles W. Whitcomb School 2.50732e+11
    ## 2    Washington WA              10th Street School 5.30486e+11
    ## 3     Minnesota MN       112 ALC Independent Study 2.70819e+11
    ## 4     Minnesota MN           112 ALC MIDDLE SCHOOL 2.70819e+11
    ## 6      Michigan MI          12th Street Elementary 2.62895e+11
    ## 8      Nebraska NE                   1R ELEMENTARY 3.17458e+11
    ##                           Agency_Name      County_Name COUNTY.ID
    ## 1                         Marlborough Middlesex County     25017
    ## 2          Marysville School District Snohomish County     53061
    ## 3 EASTERN CARVER COUNTY PUBLIC SCHOOL    Carver County     27019
    ## 4 EASTERN CARVER COUNTY PUBLIC SCHOOL    Carver County     27019
    ## 6              Portage Public Schools Kalamazoo County     26077
    ## 8            NORTHWEST PUBLIC SCHOOLS      Hall County     31079
    ##   Urban.centric.Locale Latitude  Longitude                Title.I.School.Status
    ## 1     21-Suburb: Large 42.35735  -71.54859          5-Title I schoolwide school
    ## 2  22-Suburb: Mid-size 48.05744 -122.18199               6-Not a Title I school
    ## 3     21-Suburb: Large 44.84528  -93.57477 2-Title I targeted assistance school
    ## 4     21-Suburb: Large 44.84161  -93.59692 2-Title I targeted assistance school
    ## 6       13-City: Small 42.22038  -85.64661               6-Not a Title I school
    ## 8     41-Rural: Fringe 41.00278  -98.37956               6-Not a Title I school
    ##   Updated.Status
    ## 1         1-Open
    ## 2         1-Open
    ## 3         1-Open
    ## 4         1-Open
    ## 6         1-Open
    ## 8         1-Open
    ##                                                                      Agency.Type
    ## 1 1-Regular local school district that is NOT a component of a supervisory union
    ## 2 1-Regular local school district that is NOT a component of a supervisory union
    ## 3 1-Regular local school district that is NOT a component of a supervisory union
    ## 4 1-Regular local school district that is NOT a component of a supervisory union
    ## 6 1-Regular local school district that is NOT a component of a supervisory union
    ## 8 1-Regular local school district that is NOT a component of a supervisory union
    ##                  School.Type Members Male Female FTE.Equivalent
    ## 1           1-Regular school    1308  681    627          118.8
    ## 2           1-Regular school     178   77    101            7.3
    ## 3 4-Alternative/other school      37   20     17           <NA>
    ## 4 4-Alternative/other school     366  198    168           <NA>
    ## 6           1-Regular school     530  269    261          29.26
    ## 8           1-Regular school     182   96     86          12.65
    ##   Pupil.Teacher.Ratio
    ## 1               11.01
    ## 2               24.38
    ## 3                 n/a
    ## 4                 n/a
    ## 6               18.11
    ## 8               14.39

\#\#Task 3.2 FTE and Pupil Teacher Ratio

### Convert FTE and Pupil Teacher Ratio to numeric

``` r
public_school$FTE.Equivalent <- as.numeric(as.character(public_school[,18]))
```

``` r
public_school$Pupil.Teacher.Ratio <- as.numeric(as.character(public_school[,19]))
```

    ## Warning: NAs introduced by coercion

Note that original n/a - not applicable values are now replaces with
NAs, which is not a problem since we don’t have information on that
either.

``` r
head(public_school)
```

    ##      State.Name ST                     School.Name  NCES.SchID
    ## 1 Massachusetts MA 1 LT Charles W. Whitcomb School 2.50732e+11
    ## 2    Washington WA              10th Street School 5.30486e+11
    ## 3     Minnesota MN       112 ALC Independent Study 2.70819e+11
    ## 4     Minnesota MN           112 ALC MIDDLE SCHOOL 2.70819e+11
    ## 6      Michigan MI          12th Street Elementary 2.62895e+11
    ## 8      Nebraska NE                   1R ELEMENTARY 3.17458e+11
    ##                           Agency_Name      County_Name COUNTY.ID
    ## 1                         Marlborough Middlesex County     25017
    ## 2          Marysville School District Snohomish County     53061
    ## 3 EASTERN CARVER COUNTY PUBLIC SCHOOL    Carver County     27019
    ## 4 EASTERN CARVER COUNTY PUBLIC SCHOOL    Carver County     27019
    ## 6              Portage Public Schools Kalamazoo County     26077
    ## 8            NORTHWEST PUBLIC SCHOOLS      Hall County     31079
    ##   Urban.centric.Locale Latitude  Longitude                Title.I.School.Status
    ## 1     21-Suburb: Large 42.35735  -71.54859          5-Title I schoolwide school
    ## 2  22-Suburb: Mid-size 48.05744 -122.18199               6-Not a Title I school
    ## 3     21-Suburb: Large 44.84528  -93.57477 2-Title I targeted assistance school
    ## 4     21-Suburb: Large 44.84161  -93.59692 2-Title I targeted assistance school
    ## 6       13-City: Small 42.22038  -85.64661               6-Not a Title I school
    ## 8     41-Rural: Fringe 41.00278  -98.37956               6-Not a Title I school
    ##   Updated.Status
    ## 1         1-Open
    ## 2         1-Open
    ## 3         1-Open
    ## 4         1-Open
    ## 6         1-Open
    ## 8         1-Open
    ##                                                                      Agency.Type
    ## 1 1-Regular local school district that is NOT a component of a supervisory union
    ## 2 1-Regular local school district that is NOT a component of a supervisory union
    ## 3 1-Regular local school district that is NOT a component of a supervisory union
    ## 4 1-Regular local school district that is NOT a component of a supervisory union
    ## 6 1-Regular local school district that is NOT a component of a supervisory union
    ## 8 1-Regular local school district that is NOT a component of a supervisory union
    ##                  School.Type Members Male Female FTE.Equivalent
    ## 1           1-Regular school    1308  681    627         118.80
    ## 2           1-Regular school     178   77    101           7.30
    ## 3 4-Alternative/other school      37   20     17             NA
    ## 4 4-Alternative/other school     366  198    168             NA
    ## 6           1-Regular school     530  269    261          29.26
    ## 8           1-Regular school     182   96     86          12.65
    ##   Pupil.Teacher.Ratio
    ## 1               11.01
    ## 2               24.38
    ## 3                  NA
    ## 4                  NA
    ## 6               18.11
    ## 8               14.39

#### Task 3.3 Review incomplete cases at this point

``` r
incomplete_sch <- public_school[!complete.cases(public_school),]
# There are about 3989 schools that didn't have the FTE information or Pupil to Teacher Ratio. However those are not necessary needed in the next step. We can move forward this this dataframe.
head(incomplete_sch)
```

    ##     State.Name ST                      School.Name  NCES.SchID
    ## 3    Minnesota MN        112 ALC Independent Study 2.70819e+11
    ## 4    Minnesota MN            112 ALC MIDDLE SCHOOL 2.70819e+11
    ## 51      KANSAS KS                        500 Reach 2.00795e+11
    ## 108   Illinois IL          A J Katzenmaier Academy 1.70011e+11
    ## 129   Illinois IL         A O Marshall Elem School 1.72058e+11
    ## 163  Louisiana LA A. E. Phillips Laboratory School 2.20006e+11
    ##                             Agency_Name      County_Name COUNTY.ID
    ## 3   EASTERN CARVER COUNTY PUBLIC SCHOOL    Carver County     27019
    ## 4   EASTERN CARVER COUNTY PUBLIC SCHOOL    Carver County     27019
    ## 51                          Kansas City Wyandotte County     20209
    ## 108                North Chicago SD 187      Lake County     17097
    ## 129                       Joliet PSD 86      Will County     17197
    ## 163     A.E. Phillips Laboratory School   Lincoln Parish     22061
    ##     Urban.centric.Locale Latitude Longitude
    ## 3       21-Suburb: Large 44.84528 -93.57477
    ## 4       21-Suburb: Large 44.84161 -93.59692
    ## 51     12-City: Mid-size 39.12919 -94.75772
    ## 108     21-Suburb: Large 42.32605 -87.86273
    ## 129     21-Suburb: Large 41.53138 -88.04488
    ## 163     32-Town: Distant 32.52588 -92.65047
    ##                    Title.I.School.Status Updated.Status
    ## 3   2-Title I targeted assistance school         1-Open
    ## 4   2-Title I targeted assistance school         1-Open
    ## 51                6-Not a Title I school          3-New
    ## 108                                 <NA>         1-Open
    ## 129                                 <NA>         1-Open
    ## 163                                 <NA>         1-Open
    ##                                                                        Agency.Type
    ## 3   1-Regular local school district that is NOT a component of a supervisory union
    ## 4   1-Regular local school district that is NOT a component of a supervisory union
    ## 51  1-Regular local school district that is NOT a component of a supervisory union
    ## 108 1-Regular local school district that is NOT a component of a supervisory union
    ## 129 1-Regular local school district that is NOT a component of a supervisory union
    ## 163                                                     8-Other education agencies
    ##                    School.Type Members Male Female FTE.Equivalent
    ## 3   4-Alternative/other school      37   20     17             NA
    ## 4   4-Alternative/other school     366  198    168             NA
    ## 51            1-Regular school     328  123    205              0
    ## 108           1-Regular school     436  228    208             28
    ## 129           1-Regular school     485  249    236             29
    ## 163           1-Regular school     397  201    196             22
    ##     Pupil.Teacher.Ratio
    ## 3                    NA
    ## 4                    NA
    ## 51                   NA
    ## 108               15.57
    ## 129               16.72
    ## 163               18.05

## Task 4. Rename factor levels and re-order levels

``` r
table(public_school$Title.I.School.Status)
```

    ## 
    ##          1-Title I targeted assistance eligible school-No program 
    ##                                                              5271 
    ##                              2-Title I targeted assistance school 
    ##                                                              9311 
    ## 3-Title I schoolwide eligible-Title I targeted assistance program 
    ##                                                              2508 
    ##                   4-Title I schoolwide eligible school-No program 
    ##                                                              6155 
    ##                                       5-Title I schoolwide school 
    ##                                                             46136 
    ##                                            6-Not a Title I school 
    ##                                                             24429

### Task 4.1 Re-organize Title I levels

Convert characters to level first

``` r
public_school$Title.I.School.Status <- as.factor(public_school$Title.I.School.Status)
```

View levels

``` r
levels(public_school$Title.I.School.Status)
```

    ## [1] "1-Title I targeted assistance eligible school-No program"         
    ## [2] "2-Title I targeted assistance school"                             
    ## [3] "3-Title I schoolwide eligible-Title I targeted assistance program"
    ## [4] "4-Title I schoolwide eligible school-No program"                  
    ## [5] "5-Title I schoolwide school"                                      
    ## [6] "6-Not a Title I school"

View dataframe

``` r
head(public_school)
```

    ##      State.Name ST                     School.Name  NCES.SchID
    ## 1 Massachusetts MA 1 LT Charles W. Whitcomb School 2.50732e+11
    ## 2    Washington WA              10th Street School 5.30486e+11
    ## 3     Minnesota MN       112 ALC Independent Study 2.70819e+11
    ## 4     Minnesota MN           112 ALC MIDDLE SCHOOL 2.70819e+11
    ## 6      Michigan MI          12th Street Elementary 2.62895e+11
    ## 8      Nebraska NE                   1R ELEMENTARY 3.17458e+11
    ##                           Agency_Name      County_Name COUNTY.ID
    ## 1                         Marlborough Middlesex County     25017
    ## 2          Marysville School District Snohomish County     53061
    ## 3 EASTERN CARVER COUNTY PUBLIC SCHOOL    Carver County     27019
    ## 4 EASTERN CARVER COUNTY PUBLIC SCHOOL    Carver County     27019
    ## 6              Portage Public Schools Kalamazoo County     26077
    ## 8            NORTHWEST PUBLIC SCHOOLS      Hall County     31079
    ##   Urban.centric.Locale Latitude  Longitude                Title.I.School.Status
    ## 1     21-Suburb: Large 42.35735  -71.54859          5-Title I schoolwide school
    ## 2  22-Suburb: Mid-size 48.05744 -122.18199               6-Not a Title I school
    ## 3     21-Suburb: Large 44.84528  -93.57477 2-Title I targeted assistance school
    ## 4     21-Suburb: Large 44.84161  -93.59692 2-Title I targeted assistance school
    ## 6       13-City: Small 42.22038  -85.64661               6-Not a Title I school
    ## 8     41-Rural: Fringe 41.00278  -98.37956               6-Not a Title I school
    ##   Updated.Status
    ## 1         1-Open
    ## 2         1-Open
    ## 3         1-Open
    ## 4         1-Open
    ## 6         1-Open
    ## 8         1-Open
    ##                                                                      Agency.Type
    ## 1 1-Regular local school district that is NOT a component of a supervisory union
    ## 2 1-Regular local school district that is NOT a component of a supervisory union
    ## 3 1-Regular local school district that is NOT a component of a supervisory union
    ## 4 1-Regular local school district that is NOT a component of a supervisory union
    ## 6 1-Regular local school district that is NOT a component of a supervisory union
    ## 8 1-Regular local school district that is NOT a component of a supervisory union
    ##                  School.Type Members Male Female FTE.Equivalent
    ## 1           1-Regular school    1308  681    627         118.80
    ## 2           1-Regular school     178   77    101           7.30
    ## 3 4-Alternative/other school      37   20     17             NA
    ## 4 4-Alternative/other school     366  198    168             NA
    ## 6           1-Regular school     530  269    261          29.26
    ## 8           1-Regular school     182   96     86          12.65
    ##   Pupil.Teacher.Ratio
    ## 1               11.01
    ## 2               24.38
    ## 3                  NA
    ## 4                  NA
    ## 6               18.11
    ## 8               14.39

``` r
table(public_school$Title.I.School.Status) 
```

    ## 
    ##          1-Title I targeted assistance eligible school-No program 
    ##                                                              5271 
    ##                              2-Title I targeted assistance school 
    ##                                                              9311 
    ## 3-Title I schoolwide eligible-Title I targeted assistance program 
    ##                                                              2508 
    ##                   4-Title I schoolwide eligible school-No program 
    ##                                                              6155 
    ##                                       5-Title I schoolwide school 
    ##                                                             46136 
    ##                                            6-Not a Title I school 
    ##                                                             24429

``` r
#Total count is 93810, which means there are missing values in the dataset 
```

``` r
#Where are the missing values in Title 1 Status?
table(is.na(public_school$Title.I.School.Status)) #There are 809 missing value in Title 1 
```

    ## 
    ## FALSE  TRUE 
    ## 93810   809

``` r
#Create a level first in order to handle missing values, otherwise it gives an error
a <- levels(public_school$Title.I.School.Status)
a[length(a) + 1] <- "Unknown"
```

``` r
#Add the level to the variables
public_school$Title.I.School.Status <- factor(public_school$Title.I.School.Status, levels = a)
#Add the level to the missing values 
public_school$Title.I.School.Status[is.na(public_school$Title.I.School.Status)] <- "Unknown" 
```

``` r
levels(public_school$Title.I.School.Status)
```

    ## [1] "1-Title I targeted assistance eligible school-No program"         
    ## [2] "2-Title I targeted assistance school"                             
    ## [3] "3-Title I schoolwide eligible-Title I targeted assistance program"
    ## [4] "4-Title I schoolwide eligible school-No program"                  
    ## [5] "5-Title I schoolwide school"                                      
    ## [6] "6-Not a Title I school"                                           
    ## [7] "Unknown"

``` r
#Check to make sure the count for each level is still correct 
table(public_school$Title.I.School.Status)
```

    ## 
    ##          1-Title I targeted assistance eligible school-No program 
    ##                                                              5271 
    ##                              2-Title I targeted assistance school 
    ##                                                              9311 
    ## 3-Title I schoolwide eligible-Title I targeted assistance program 
    ##                                                              2508 
    ##                   4-Title I schoolwide eligible school-No program 
    ##                                                              6155 
    ##                                       5-Title I schoolwide school 
    ##                                                             46136 
    ##                                            6-Not a Title I school 
    ##                                                             24429 
    ##                                                           Unknown 
    ##                                                               809

## Task 5 Aggregrate Urbanicity/Rurality information

``` r
#View Counts in each level 
table((public_school$Urban.centric.Locale))
```

    ## 
    ##      11-City: Large   12-City: Mid-size      13-City: Small    21-Suburb: Large 
    ##               14255                5508                6134               25188 
    ## 22-Suburb: Mid-size    23-Suburb: Small     31-Town: Fringe    32-Town: Distant 
    ##                3092                1807                2775                5795 
    ##     33-Town: Remote    41-Rural: Fringe   42-Rural: Distant    43-Rural: Remote 
    ##                3695               10455                9847                6068

``` r
public_school$Urban.centric.Locale <- as.factor(public_school$Urban.centric.Locale)
#View levels after conversion
levels(public_school$Urban.centric.Locale)
```

    ##  [1] "11-City: Large"      "12-City: Mid-size"   "13-City: Small"     
    ##  [4] "21-Suburb: Large"    "22-Suburb: Mid-size" "23-Suburb: Small"   
    ##  [7] "31-Town: Fringe"     "32-Town: Distant"    "33-Town: Remote"    
    ## [10] "41-Rural: Fringe"    "42-Rural: Distant"   "43-Rural: Remote"

``` r
#For this analysis, we will only be concern with the 4 major levels of locale 
levels(public_school$Urban.centric.Locale)[1:3] <- "City"
levels(public_school$Urban.centric.Locale)
```

    ##  [1] "City"                "21-Suburb: Large"    "22-Suburb: Mid-size"
    ##  [4] "23-Suburb: Small"    "31-Town: Fringe"     "32-Town: Distant"   
    ##  [7] "33-Town: Remote"     "41-Rural: Fringe"    "42-Rural: Distant"  
    ## [10] "43-Rural: Remote"

``` r
#I discovered that the levels changed after converting city.So I need to adjust the index
levels(public_school$Urban.centric.Locale)[2:4] <- "Suburb"
levels(public_school$Urban.centric.Locale)
```

    ## [1] "City"              "Suburb"            "31-Town: Fringe"  
    ## [4] "32-Town: Distant"  "33-Town: Remote"   "41-Rural: Fringe" 
    ## [7] "42-Rural: Distant" "43-Rural: Remote"

``` r
levels(public_school$Urban.centric.Locale)[3:5] <- "Town"
levels(public_school$Urban.centric.Locale)
```

    ## [1] "City"              "Suburb"            "Town"             
    ## [4] "41-Rural: Fringe"  "42-Rural: Distant" "43-Rural: Remote"

``` r
levels(public_school$Urban.centric.Locale)[4:6] <- "Rural"
levels(public_school$Urban.centric.Locale)
```

    ## [1] "City"   "Suburb" "Town"   "Rural"

``` r
table(public_school$Urban.centric.Locale)
```

    ## 
    ##   City Suburb   Town  Rural 
    ##  25897  30087  12265  26370

Urbanicity/Rurality is converted to 4 levels now
