setwd("C:/Users/venla/Documents/Yliopisto/RegVar")
getwd()

#teht 1a tallennetaan arvot pituus vektoriin
pituus <- c(165.0, 166.0, 171.0, 154.0, 166.0, 159.5, 166.5, 158.5)

#teht 1b, vaakasuora pistekuvio
library(beeswarm)
beeswarm(pituus, horizontal=TRUE)

#teht 1c, funktio, joka laskee keskivirheen
SEmean <- function(x) sd(x)/sqrt( length(x) )

#teht 1d
n <- length(pituus)
mean.pit <- mean(pituus)
sd.pit <- sd(pituus)
se.pit <- SEmean(pituus)
#tulostetaan arvot
n
mean.pit
sd.pit
se.pit

#teht 1e
mu0 <- 167
Thav <- (mean.pit - mu0)/se.pit; Thav


#teht 1f
curve(dt(x,6), from=-4, to=4)
points(Thav, 0, pch=16)


#teht 1g
Phav <- 2*( 1 - pt(abs(Thav), n-1) )
c(Thav, Phav)


#teht 1h
t <- qt(0.95, n-1)
ci <- mean.pit + c(-1,1) * t.95 * se.pit
ci
t

#teht 1i
t.test(pituus, mu=167, conf.level=0.90)


#teht 2a
u <- seq(150, 185, by=0.1) ; u
mu <- 167; sig <- 5
plot( u, dnorm(u, mu, sig), type = "l", ylim=c(0,0.1) )


#teht 2b
pnorm(160, 167, 5)
1-pnorm(175,167,5)

#teht 2c
qnorm(0.025, 167, 5)
qnorm(0.975, 167, 5)


#teht 3a
otos <- rnorm(7, mu, sig); otos
summary(otos); sd(otos); SEmean(otos)
otos
t.test(otos, mu=10, conf.level=0.95)
otos <- rnorm(1000, mu, sig); otos
summary(otos); sd(otos); SEmean(otos)

t.test(otos, mu=10, conf.level=0.95)
#teht 3d
hist(otos, freq=FALSE, breaks=seq(130,210, by=2),xlim=c(150,185), add=TRUE)


#teht 4a
source("esanfunktiot.R")
options(digits=4)

#teht 4b
otos10 <- normotos.sim(10, mu, sig)

#teht 4d
summary(otos10)


#teht 4e
otos100 <- normotos.sim(100, mu, sig, loc=F)
summary(otos100)


#teht 5a
otos7.10k <- normotos.sim(7, mu, sig, nsim=10000, kuva=FALSE, loc=FALSE)
attach(otos7.10k)
summary(otos7.10k)

#teht 5b
hist(keskiarvo, freq=F, br=150:185)
lines( u, dnorm(u, mu, sig/sqrt(7)) )
lines( u, dnorm(u, mu, sig), lty=3 )

#teht 5c
hist(hajonta, freq=FALSE)

#teht 6a
hist(T.suure, freq=F, br=seq(-20, 20, by=0.2), xlim=c(-6,6) )
tval <- seq(-6,6, by=0.1)
lines( tval, dnorm(tval), col="red" )
lines( tval, dt(tval, df=7-1), col="blue" )


#teht 6b
hist(P.arvo, freq=FALSE)


#teht 6c
length(mu.alar[ mu.alar > 167]) 
length(mu.ylar[ mu.ylar < 167])



length(mu.alar[ mu.alar > 167]) + length(mu.ylar[ mu.ylar < 167])