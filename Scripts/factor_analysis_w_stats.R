# Factor Analysis with stats package 
# <https://www.geo.fu-berlin.de/en/v/soga/Geodata-analysis/factor-analysis/A-simple-example-of-FA/index.html>
library(stats)

sdo <- read.csv("./Datasets/factor_score.csv", header = T, fileEncoding="UTF-8-BOM")

colnames(sdo)
head(sdo)

# ------ Create a composite indicator with R factor analysis -------
str(sdo)

composite_1 <- sdo[,c(19:22, 24:26,32:33)]

colnames(composite_1)

# Principal Axis Factor Analysis with base R stats package

# Model 1: Three factors assumption with default rotation and no scores
model_1 <- factanal(composite_1, factors = 3) 
# Review initial model 1 result 
model_1
# Communality
apply(model_1$loadings^2,1,sum)
# uniqueness 
1-apply(model_1$loadings^2,1,sum)


# Model 2: Three factors with varimax rotation and Bartlett scores
model_2 <-factanal(composite_1, factors = 3, rotation = 'varimax', score = "Bartlett")
# Review initial model 1 result 
model_2
# Communality
apply(model_2$loadings^2,1,sum)
# uniqueness 
1-apply(model_2$loadings^2,1,sum)

# Model 3: Three factors with varimax rotation and Regression scores
model_3 <-factanal(composite_1, factors = 3, rotation = 'promax', score = "regression")
# Review initial model 1 result 
model_3
# Communality
apply(model_3$loadings^2,1,sum)
# uniqueness 
1-apply(model_3$loadings^2,1,sum)