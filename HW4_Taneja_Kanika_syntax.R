#HW 4

#question 1
regGas <- c(16, 20, 21, 22, 23, 22, 27, 25, 27, 28)
premGas <- c(19, 22, 24, 24, 25,25, 26, 26, 28, 32)
t.test(regGas, premGas, pooled= F, paired=T, var.equal = T )

#question 2a

comic<- read.csv(file.choose(), header=T)
attach(comic)
summary(Worldwide[Studio=="Marvel"]) #returns min, max, mean,median, Q1, Q3
summary(Worldwide[Studio=="DC"])
summary(Worldwide) #summary of both combined
length(Worldwide[Studio=="Marvel"]) #returns n
length(Worldwide[Studio=="DC"]) #returns n
length(Worldwide) #returns n
sd(Worldwide[Studio=="Marvel"], na.rm=T) # std.dev
sd(Worldwide[Studio=="DC"], na.rm=T) # std.dev
sd(Worldwide, na.rm=T)   


#Question 2b

t.test(Worldwide[Studio=="Marvel"], Worldwide[Studio=="DC"], conf= .90) #t-test

library(effsize) #activates effsize library
cohen.d(Worldwide[Studio=="Marvel"], Worldwide[Studio=="DC"]) #cohen's d score

#Question 3
library(car)
preDisney <- Review[Studio=="Marvel"& Year<="2009"]
currDisney <-Review[Studio=="Marvel"& Year>="2010"]
summary(preDisney) #summary stat
summary(currDisney)
length(preDisney) #returns n
length(currDisney)
sd(currDisney) #st dev
sd(preDisney)
t.test(preDisney, currDisney, conf=.90) #t-test

#Question 4B
library(pwr) 
pwr.t.test(n=126, d=.5, sig.level=.05, power=NULL, alternative="two.sided", type="two.sample") #finding power value


