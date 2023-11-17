setwd("C:/Users/venla/Documents/Yliopisto/RegVar")
getwd()

#teht 1a
elinaika <- c(4, 5, 10, 11, 20, 29, 35, 40, 66, 70)

#teht 1b
library(beeswarm)
beeswarm(elinaika, horizontal=TRUE)

#teht 1c
stripchart(elinaika, vertical = TRUE)

#teht 1d
summary(elinaika) ; round(sd(elinaika), 1)

#teht 1e
SEmean <- function(x) sd(x)/sqrt( length(x) )
n <- length(elinaika)
mean.pit <- mean(elinaika)
sd.pit <- sd(elinaika)
se.pit <- SEmean(elinaika)
n
mean.pit
sd.pit
se.pit

t.test(elinaika, mu=40, conf.level=0.95)


#teht 2a
malli <- lm(elinaika ~ 1)

#teht 2b
sovitteet <- fitted(malli) ; sovitteet 
residuaalit <- resid(malli) ; residuaalit
data.frame(elinaika, sovitteet, residuaalit)

#teht 2c
mean.residuaalit <- mean(residuaalit)
mean.residuaalit
sd.residuaalit <- sd(residuaalit)
sd.residuaalit

#teht 2d
mean.sovitteet <- mean(sovitteet)
mean.sovitteet
sd.sovitteet <- sd(sovitteet)
sd.sovitteet


#teht 3a
summary(malli)
sd(elinaika)/sqrt(length(elinaika))
t.test(elinaika, mu=0, conf.level=0.95)

#teht 3b
confint(malli, level = 0.99)


#teht 4a
n <- length(elinaika) ; n
i <- 1:n ; i
nu <- i/(n+1) ; nu
z.k <- qnorm(nu) ; z.k

qnorm(1/(n+1))
z.k[1]
plot(z.k, elinaika, main="QQ-kuvio")


#teht 4b
qqnorm(elinaika)
qqline(elinaika)

#teht 4c
par(mfrow=c(2,1))
qqnorm(elinaika) ; qqline(elinaika)
qqnorm(residuaalit) ; qqline(residuaalit)

#teht 5a
syke <- read.table("sykkeet.txt", header = TRUE)
str(syke)

#teht 5b
syke$ryhma <- factor(syke$ryhma, labels = c(' vertailu', ' koe') )
summary(syke)
attach(syke)


#teht 5c
source("Esanfunktiot.R")
tunnus.taulu
tunnus.taulu(loppusyke, ryhma, 2)

#teht 5d
par(mfrow=c(1,1))
beeswarm( loppusyke ~ ryhma, horizontal = TRUE)
boxplot( loppusyke ~ ryhma, horizontal = TRUE, add = TRUE)


#teht 6a
ls.karvot <- tapply(loppusyke, ryhma, mean)
ls.karvot
ls.karvot[2] - ls.karvot[1]
ls.karvot[1] - ls.karvot[2] 
points( ls.karvot, c(1,2), pch = 16, cex = 1.5)

#teht 6b
t.test(loppusyke ~ ryhma, var.equal=TRUE)


#teht 6c
lm1 <- lm(loppusyke ~ ryhma)
summary(lm1)

#teht 6d
confint(lm1)


#teht 6e
sov <- fitted(lm1); sov Moodle 18
res <- resid(lm1)
data.frame(loppusyke, sov, res)


#teht 6f
par(mfrow=c(1,2))
plot(lm1, 1:2)
