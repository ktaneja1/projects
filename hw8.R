# this data comes from: http://archive.ics.uci.edu/ml/datasets/Bike+Sharing+Dataset 
library(car) # for recode & vif functions
bike <- read.csv(file.choose(), header=T)  # load day.csv
attach(bike)
# un-normalize select variables
bike$atemp.u <- bike$atemp *50
bike$temp.u <- bike$temp *41
bike$hum.u  <- bike$hum *100
bike$wind.u <- bike$wind *67
# create seasonal subsets
bike.spring <- subset(bike, bike$season==1)
bike.summer <- subset(bike, bike$season==2)
bike.fall <- subset(bike, bike$season==3)
bike.winter <- subset(bike, bike$season==4)

################# Question 1a #######################
one_a <- lm(bike$cnt ~ factor(bike$season) + bike$mnth + bike$holiday + bike$workingday 
            + bike$temp.u + bike$hum.u + bike$wind.u)
summary(one_a)


################# Question 1c #######################
plot(one_a)

################# Question 1d #######################

#Creating dummies
remove.packages("car")
install.packages("car")
library(car)
dummy_spring <- recode(as.numeric(bike$season), "1=1; 2:4=0")
dummy_summer <- recode(as.numeric(bike$season), "1=0; 2=1; 3:4=0")
dummy_fall <- recode(as.numeric(bike$season), "1:2=0; 3=1; 4=0")
dummy_winter <- recode(as.numeric(bike$season), "1:3=0; 4=1")


table(bike$season, dummy_summer) #checking table for correctness

one_d <- lm(bike$cnt ~ dummy_summer + dummy_fall + dummy_winter +  bike$mnth + 
              bike$holiday + bike$workingday  + bike$temp.u + bike$hum.u + bike$wind.u)


vif(one_d)
################# Question 1e #######################

#Spring
summary(lm(cnt ~factor(mnth) + holiday + workingday 
           + temp.u + hum.u + wind.u, data= bike.spring))
#Summer
summary(lm(cnt ~factor(mnth) + holiday + workingday 
           + temp.u + hum.u + wind.u, data= bike.summer))
#Fall
summary(lm(cnt ~factor(mnth) + holiday + workingday 
           + temp.u + hum.u + wind.u, data= bike.fall))
#Winter
summary(lm(cnt ~factor(mnth) + holiday + workingday 
           + temp.u + hum.u + wind.u, data= bike.winter))

################# Question 1k #######################
bike[469,]

################# Question 1m #######################
fahrenheit <- (bike$temp.u * (9/5))+24

cor.test(bike$temp.u, fahrenheit)


