## Factor Analysis
# https://www.geo.fu-berlin.de/en/v/soga/Geodata-analysis/factor-analysis/A-simple-example-of-FA/index.html

mlmdata <- read.csv("data_imputed.csv", header = T)

names(mlmdata)
head(mlmdata)

mlmdata[,26]<-100-mlmdata[,26]

#Create a composite indicator 


# comp <- mlmdata[,c(19:22,26,29:30)]
comp <- mlmdata[,c(19,20,24,26,30)]

names(comp)

#library(psch)
# fa()

library(GPArotation)

#Principal Axis Factor Analysis with base R stats package 
model <- factanal(comp, factors = 2, rotation="oblimin", scores='regression')
model

model <- fa(comp, nfactors = 2, scores='Anderson', residuals=TRUE)
model

summary(model$residual)


names(model)

head(model$scores)
summary(model$scores)

library(aspect)


cor.mat<-corAspect(comp,  aspect = "aspectSum", pow = 2)
plot(cor.mat, plot.type="transplot")


names(cor.mat)

scores.cor.mat<-cor.mat$scoremat
scores.cor.mat
pairs(scores.cor.mat)

library(psych)
comp.fit<-fa(scores.cor.mat, rotation="varimax", scores="Anderson", 
             nfactors=2, residuals=TRUE, fm="ml")

comp.fit<-omega(scores.cor.mat, rotation="oblimin", 
                scores="regression", nfactors=2, residuals=TRUE, fm="ml")
comp.fit

cor(comp.fit$scores)


comp.fit

names(comp.fit)

summary(comp.fit$residual)
summary(model$residual)

head(comp.fit$scores)
pairs(comp.fit$scores)
pairs(model$scores)

cor(scores.cor.mat)
cor(comp)

pairs(cbind(scores.cor.mat, comp.fit$scores))


model

names(model)

#Extract Factor Score
#http://bayes.acs.unt.edu:8083/BayesContent/class/Jon/R_SC/Module4/CompositeIndicators.R 

#convert to vector and use summary, other wise you get a long list of scores 

head(model$scores)

summary(as.vector(model$scores))


apply(model$loadings^2,1,sum)

library(radiant)

# weighted.sd (x, wt, na.rm = TRUE)
# Number of vector, number of weights()

# This re-scaling function simply puts the scores back into the metric of the
# original questions (Keep in mind, some of the final scores may be slightly
# below '1' and some slightly above '4'; this results because we modeled the
# latent 'true scores'.

re.scale <- function(f.scores, raw.data, loadings){
  fz.scores <- (f.scores + mean(f.scores))/(sd(f.scores))
  means <- apply(raw.data, 1, weighted.mean, w = loadings)
  sds <- apply(raw.data, 1, weighted.sd, w = loadings)
  grand.mean <- mean(means)
  grand.sd <- mean(sds)
  final.scores <- ((fz.scores * grand.sd) + grand.mean)
  return(final.scores)}


#raw data
df <- comp[,-c(3,4)]
#Assigned factor scores to a variable
model_score <- as.vector(model$scores)
#Assigned model loading to a variable
model_loadings <- abs(model$loadings[,1])

model_final_score <- re.scale(model_score, df, model_loadings)
summary(model_final_score)

# http://bayes.acs.unt.edu:8083/BayesContent/class/Jon/R_SC/Module4/CompositeIndicators.R


get.scores.fun <- function(data){
  fact <- factanal(data, factors = 1, scores = "regression")
  f.scores <- fact$scores[,1]
  f.loads <- fact$loadings[,1]
  rescaled.scores <- re.scale(f.scores, data, f.loads)
  output.list <- list(rescaled.scores, f.loads)
  names(output.list) <- c("rescaled.scores","factor.loadings")
  return(output.list)
}

b <- get.scores.fun(df)
b





