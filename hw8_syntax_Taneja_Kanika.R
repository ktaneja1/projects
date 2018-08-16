# HW8
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

#Question 1A
bikes <- lm(formula=cnt ~ factor(season)+mnth+ holiday + workingday + temp.u + 
              hum.u+wind.u, data=bike)
summary(bikes)


#Question 1c
plot(bikes)

#Question 1d
dum.spring <- recode(as.numeric(season), "1=1; 2:4=0")
dum.summer <- recode(as.numeric(season), "1=0; 2=1; 3:4=0")
dum.fall <- recode(as.numeric(season), "1:2=0; 3=1; 4=0")
dum.winter <- recode(as.numeric(season), "1:3=0; 4=1")

lm.bike <- lm(cnt ~ dum.summer + dum.fall + dum.winter + mnth + 
                holiday + workingday + temp.u + hum.u + 
                wind.u, data = bike)
vif(lm.bike)

#question 1e

#spring
summary(lm(cnt~factor(mnth)+holiday+workingday 
           +temp.u+hum.u+wind.u, data= bike.spring))
#fall
summary(lm(cnt~factor(mnth)+holiday+workingday 
           +temp.u+hum.u+wind.u,data= bike.fall))
#summer
summary(lm(cnt~factor(mnth)+holiday+workingday 
           +temp.u+hum.u+wind.u, data= bike.summer))

#winter
summary(lm(cnt~factor(mnth)+holiday+workingday 
           +temp.u+hum.u+wind.u,data= bike.winter))

#1K
bike[469,]

#1M
f <-(temp.u*(9/5))+24
cor.test(temp.u,f)




