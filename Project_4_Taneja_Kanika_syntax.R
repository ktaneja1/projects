load(file.choose())
attach(gss)
library(car) #used for recoding and regression diagnostics & extra credit
library(summarytools)
library (ggplot2) #used for extra credit


#4 IV's
cbind(table(HAPPY)) #needs dummy
cbind(table(HEALTH)) #needs dummy
cbind(table(FINRELA))
cbind(table(STRESS))


table (HAPPY)  #frequency table
length(HAPPY)  	# n, variable sample size


#DV
cbind(table(MNTLHLTH))

#recoding 

#recoding mntlhlth
cbind(table(unclass(MNTLHLTH))) #frequency table od coded values in single col
#recode mntlhlth so coded value matches response
mentalhealth2= as.numeric (MNTLHLTH)-1
cbind(table(mentalhealth2))

#recoding 
cbind(table(as.numeric(FINRELA)))
cbind(table(unclass(FINRELA)))


cbind(unclass(table(gss$HAPPY)))

#recode HAPPY
# k = number of categories, so k = 3
# create 3 0/1 dummies
gss$happy.very   <- recode(as.numeric(gss$HAPPY), "1=1; 2=0; 3=0") # very happy = 1, else = 0
gss$happy.pretty <- recode(as.numeric(gss$HAPPY), "1=0; 2=1; 3=0") # pretty happy = 1, else = 0
gss$happy.not  <- recode(as.numeric(gss$HAPPY), "1=0; 2=0; 3=1") # not too happy = 1, else = 0

# check recodes
table(gss$HAPPY, gss$happy.very)    # very happy
table(gss$HAPPY, gss$happy.pretty)  # pretty happy
table(gss$HAPPY, gss$happy.not)     # not too happy

#recode HEALTH
gss$health.excellent   <- recode(as.numeric(gss$HEALTH), "1=1; 2=0; 3=0; 4=0") # excellent health = 1, else = 0
gss$health.good <- recode(as.numeric(gss$HEALTH), "1=0; 2=1; 3=0; 4=0") # good health = 1, else = 0
gss$health.fair  <- recode(as.numeric(gss$HEALTH), "1=0; 2=0; 3=1; 4=0") # fair happy = 1, else = 0
gss$health.poor  <- recode(as.numeric(gss$HEALTH), "1=0; 2=0; 3=0; 4=1") # poor happy = 1, else = 0

#check recodes
table(gss$HEALTH, gss$health.excellent)    # excellent health
table(gss$HEALTH, gss$health.good)  # pretty happy
table(gss$HEALTH, gss$health.fair)     # fairly healthy
table(gss$HEALTH, gss$health.poor)  #poor health



# regression with auto-factor approach for HEALTH & HAPPY
reg.menthealth <- lm(as.numeric(MNTLHLTH) ~ factor(HEALTH) + factor(HAPPY) + FINRELA + STRESS, data = gss)
summary(reg.menthealth)


#summary for all variables


length(HAPPY[!is.na(HAPPY)])
length(HEALTH[!is.na(HEALTH)]) 
length(FINRELA[!is.na(FINRELA)])
length(STRESS[!is.na(STRESS)])
length(MNTLHLTH[!is.na(MNTLHLTH)])

summarytools::freq(HAPPY)


#gives min, Q1, Median, Mean, Q3, Max
summary(as.numeric(HAPPY))
summary(as.numeric(HEALTH))
summary(as.numeric(FINRELA))
summary(as.numeric(STRESS))
summary(as.numeric(MNTLHLTH))

sd(as.numeric(HAPPY)) 
sd(as.numeric((HEALTH)))

#finding st. dev.
sd(MNTLHLTH, na.rm = T)
sd(HAPPY, na.rm=T)
sd(FINRELA, na.rm = T)
sd(STRESS, na.rm=T)
sd(HEALTH, na.rm=T)


##checking assumptions
summary(reg.menthealth)

plot(reg.menthealth) #plots for regression assumptions

hist(as.numeric(MNTLHLTH)) #makes histogram to check variability


#Extra Credit
numStress <- as.numeric(STRESS)
numMental <- as.numeric(MNTLHLTH)
numHappy <- as.numeric(HAPPY)
numHealth <- as.numeric(HEALTH)
numIncome <- as.numeric(FINRELA)

cor.test(numStress, numMental)
cor.test(numHappy, numMental)
cor.test (numHealth, numMental)
cor.test (numIncome, numMental)

#Mental health VS Stress

plot(as.numeric(MNTLHLTH) ~ jitter(as.numeric(STRESS)), 
pch=16,
cex= 1.3,
col= "red", 
main= "Stress VS Mental Health",  #plot title
xlab= "Work stress (1= always, 5= never)", #title of x-axis
ylab= "Days of mental health issues") #title of y-axis
abline(lm(MNTLHLTH) ~(numStress))

#Mental health vs happiness
plot(as.numeric(MNTLHLTH) ~ jitter(as.numeric(HAPPY)), 
     pch=16,
     cex= 1.3,
     col= "red", 
     main= "Happiness VS Mental Health",  #plot title
     xlab= "Happiness level (1= always, 5= never)", #title of x-axis
     ylab= "Days of mental health issues") #title of y-axis
abline(lm(MNTLHLTH) ~(numHappy))

#Mental health vs HEALTH
plot(as.numeric(MNTLHLTH) ~ jitter(as.numeric(HEALTH)), 
     pch=16,
     cex= 1.3,
     col= "red", 
     main= "Health VS Mental Health",  #plot title
     xlab= "health level (1= excellent, 5= poor)", #title of x-axis
     ylab= "Days of mental health issues") #title of y-axis
abline(lm(MNTLHLTH) ~(numHealth))


#Mental health vs HEALTH
plot(as.numeric(MNTLHLTH) ~ jitter(as.numeric(FINRELA)), 
     pch=16,
     cex= 1.3,
     col= "red", 
     main= "Opinion of income VS Mental Health",  #plot title
     xlab= "income level (1= far below average, 5= far above average)", #title of x-axis
     ylab= "Days of mental health issues") #title of y-axis
abline(lm(MNTLHLTH) ~(numIncome))






  









