#Harjoitus 5

#teht 1a
sato1 <- c(5.0, 4.3, 5.9)
sato2 <- c(6.1, 5.7, 7.0)
sato <- c(sato1, sato2)

#teht 1b
fung <- gl(2, 3, 6, labels = c(" ei", " kyllä") ) ; fung
tila <- gl(3, 1, 6) ; tila
data.frame(sato, fung, tila)

#teht 1c
interaction.plot(fung, tila, sato, type = 'b', lty=1, pch = 0:2 )

#teht 1d
ero <- sato2 - sato1 ; ero
mean(ero); sd(ero)
t.test(ero)


#teht 1f
t.test(sato2, sato1, var.equal=T)
2.8710007+0.4710007


#teht 2a
va2 <- lm(sato ~ fung + tila)
round( cbind( summary(va2)$coef , confint(va2) ), 4)


#teht 2b
sovite <- fitted(va2)
jaannos <- resid(va2)
data.frame(fung, tila, sato, sovite, jaannos)
anova(va2)


#teht 3a
sipuli.g <- c(371, 378, 464, 463, # käsittely 1: 3 riviä
+ 410, 373, 498, 654, # käsittely 2: 4 riviä
+ 642, 536, 507, 588) # käsittely 3: 5 riviä


#teht 3b
library(gmodels)
Y <- sipuli.g/1000 # vasteen mittayksiköksi kg/m^2
rivit <- gl(3,4,12)
levels(rivit) <- c(" 3r", " 4r", " 5r")
lohko <- gl(4,1,12)
data.frame(rivit, lohko, Y)


#teht 3c
Ym.r <- tapply(Y, rivit, mean) # vasteen keskiarvot käsittelyryhmissä
Ysd.r <- tapply(Y, rivit, sd) # keskihajonnat ryhmittäin
round(cbind(keskiarvo=Ym.r, hajonta=Ysd.r), 3) # siisti tulostus
round(tapply(Y, lohko, mean), 3) # lohkokeskiarvot
round(tapply(Y, lohko, sd), 3) # lohkohajonnat
round(mean(Y), 3) # kokonaiskeskiarvo

#teht 3d
interaction.plot(rivit, lohko, Y, type = 'o', pch = 1, xlab= "Rivien lkm", ylab="Satotulos (kg/m^2)" )
lines( 1:3, Ym.r, type="o", pch=4, cex = 1.5, lwd=2, col='blue')

#teht 4
m1 <- lm(Y ~ rivit)
summary(m1) 
confint(m1)
anova(m1)


#teht 5
m2 <- lm(Y ~ rivit + lohko)
summary(m2) 
confint(m2)
anova(m2)


#teht 6a
library(gmodels)
fit.contrast(m1, rivit, c(-1, 0, 1), conf.int = 0.95)


#teht 6b
 fit.contrast(m2, rivit, c(-1, 0, 1), conf.int = 0.95)














