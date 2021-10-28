


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





