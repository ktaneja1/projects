load(file.choose())
summarytools::freq(gss$POLVIEWS)
table(gss$POLVIEWS)
summarytools::freq(gss$UNIONSBD)
summarytools::freq(gss$RACE)

attach(gss)
library(car)
leveneTest(as.numeric(POLVIEWS) ~ RACE * UNIONSBD)  #we didnt fail the levenes test 

qqnorm(as.numeric(POLVIEWS))
qqline(as.numeric(POLVIEWS))

summary(aov(as.numeric(POLVIEWS) ~ RACE* UNIONSBD))
a.p <- (aov(as.numeric(POLVIEWS) ~ RACE* UNIONSBD))
summary(aov(as.numeric(POLVIEWS) ~ RACE + UNIONSBD + RACE: UNIONSBD))

library (sjstats)
# on the scale- 1= more liberal, 7= more conservative
eta_sq(a.p, partial= T)
TukeyHSD(a.p, which= "RACE")  #whites are more conservative than blacks, on the 7 pt scale there's not much difference between the 2 groups
TukeyHSD(a.p, which= "UNIONSBD") #if you're more pro union then you're also more liberal




