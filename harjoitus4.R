setwd("C:/Users/venla/Documents/Yliopisto/RegVar")
getwd()

#teht 1a
syke <- read.table("sykkeet.txt", header=T)
str(syke)

#teht 1b
syke$ryhma <- factor(syke$ryhma, labels = c(' vertailu', ' koe') )
summary(syke)
attach(syke)

#teht 1c
source("Esanfunktiot.R")
tunnus.taulu(sykemuutos, ryhma, 2)

#teht 1d
library(beeswarm)
par(mfrow=c(1,1))
beeswarm( sykemuutos ~ ryhma, horizontal = T)
boxplot( sykemuutos ~ ryhma, horizontal = T, add = T)


#teht 2a
ls.karvot <- tapply(sykemuutos, ryhma, mean)
ls.karvot
points( ls.karvot, c(1,2), pch = 16, cex = 1.5)


#teht 2b
t.test(sykemuutos ~ ryhma, var.equal=TRUE)


#teht 2c
lm1 <- lm(sykemuutos ~ ryhma)
summary(lm1)
confint(lm1)


#teht 2d
anova(lm1)
par(mfrow=c(1,2))
plot(lm1, 1:2)
(length(sykemuutos)-1) * var(sykemuutos)
sqrt(42.07)

#teht 2e
t.test( sykemuutos ~ ryhma, var.equal=F)

#teht 2f
detach(syke)


#teht 3a
mat <- read.table("mat-koe.txt", header=T)
str(mat)
summary(mat) 
attach(mat)
str(mat)
mat$ryhma <- as.factor(mat$ryhma)

#teht 3b
par(mfrow=c(1,1))
beeswarm(pisteet ~ ryhma, method='center')


#teht 3c
tunnus.taulu(pisteet, ryhma, 2)


#teht 4a
m.a <- lm( pisteet ~ ryhma - 1)
round( cbind( summary(m.a)$coef, confint(m.a) ), 2)


#teht 4b
m.b <- lm( pisteet ~ ryhma)
round( cbind( summary(m.b)$coef, confint(m.b) ), 2)

#teht 5a
yhat <- fitted.values(m.b)
res <- residuals(m.b)
data.frame(ryhma, pisteet, yhat = round(yhat, 2), res = round(res,2)) 
points( as.numeric(ryhma) , yhat, pch = 16, cex = 1.5)

#teht 5b
anova(m.b)

#teht 5c
par(mfrow=c(1,2)) #kaksi kuvaa rinnakkain
plot(m.b, which=1:2)



#teht 6a
install.packages("gmodels",dependencies = TRUE)
library(gmodels)
fit.contrast(m.b, ryhma, coeff = c(-1, -1, 1, 1)/2, conf.int=0.95 )


#teht 6b
fit.contrast(m.b, ryhma, coeff = c(0, 0, 1, -1)/2, conf.int=0.95 )








