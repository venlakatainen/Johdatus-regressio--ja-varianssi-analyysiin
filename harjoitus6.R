setwd("C:/Users/venla/Documents/Yliopisto/RegVar")
getwd()

#teht 1a
koulu <- c(6, 12, 10, 8, 9)
tulot <- c(10, 20, 17, 12, 11)


#teht 1b
par(mfrow=c(1,1))
plot( tulot ~ koulu, pch=16, xlim=c(5,13), ylim=c(8,22) )


#teht 1c
mean(koulu)
mean(tulot)

sd(koulu)
sd(tulot)

cor(koulu,tulot)

#teht 1d
m1 <- lm( tulot ~ koulu )

#teht 1e
summary(m1)
anova(m1)


#teht 1f
round( cbind( coef(m1), confint(m1) ), 2)

#teht 2a
yhat <- fitted(m1)
res <- resid(m1)
data.frame(koulu, tulot, yhat, res)

#teht 2b
points( yhat ~ koulu) # lisätään kuvaan sovitteet
abline(m1) # mallin m1 mukainen sovitettu regressiosuora 
segments( koulu, tulot, koulu, yhat, lty=3 ) # pystysuorat etäisyydet

#teht 2c
xnew <- data.frame( koulu = seq(6,12, by=0.5) ) ; xnew

#teht 2d
yfit <- predict(m1, newdata= xnew, interval="confidence", level=0.95)
round(cbind(xnew, yfit), 2)
24.54-13.96

#teht 2e
lines( xnew[, 1], yfit[, 2], lty=2)
lines( xnew[, 1], yfit[, 3], lty=2)

#teht 2f
ypred <- predict(m1, newdata= xnew, interval="predict", level=0.95)
round(cbind(xnew, ypred), 2)
lines( xnew[, 1], ypred[, 2], lty=2)
lines( xnew[, 1], ypred[, 3], lty=2)

#teht 3a
brain <- read.table("brain.txt", header=T)
str(brain)

#teht 3b
brain$sukup <- factor(brain$suku, levels=c(0,1), labels = c("mies", "nainen") )
attach(brain)
source("Esanfunktiot.r") # funktio kopioitu Moodlesta R-harjoituksessa 2
tunnus.taulu(IQ, sukup, 1)
tunnus.taulu(MRI, sukup, 2)
tunnus.taulu(pituus, sukup, 1)


#teht 3c
brain.m <- subset(brain, sukup == "mies") ; brain.m
brain.n <- subset(brain, sukup == "nainen") ; brain.n


#teht 3d
par(mfrow=c(1,2))
with(brain.m, plot(IQ ~ MRI, pch = 16, main = "Miehet") )
with(brain.n, plot(IQ ~ MRI, pch = 16, main = "Naiset") )

#teht 3e
with( brain.m, cor(MRI, IQ) )
with( brain.n, cor(MRI, IQ) )

#teht 4a
malli.n <- lm( IQ ~ MRI, data = brain.n)
summary(malli.n)
round( cbind( coef(malli.n), confint(malli.n) ), 2)
anova(malli.n)
confint(malli.n, level=0.99)


#teht 4b
par(mfrow=c(1,1))
with(brain.n, plot( IQ ~ MRI, pch = 16) )
abline(malli.n)

#teht 4c
brain.n$MRI.kesk <- brain.n$MRI-mean(brain.n$MRI)
brain.n$MRI.kesk

mean(brain.n$MRI); mean(brain.n$MRI.kesk)
sd(brain.n$MRI); sd(brain.n$MRI.kesk)

#teht 4d
malli2.n <- lm( IQ ~ MRI.kesk, data = brain.n)
summary(malli2.n)
round( cbind( coef(malli2.n), confint(malli2.n) ), 2)

(23.75/110.45)*100

33.41+2,29
120.18-100.72 

130.54+178.03
19.46/308.57

15.556/110.450
0.140842*100

anova(malli2.n)




