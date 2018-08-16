load(file.choose())
attach(gss)
install.packages("car") #installs package, only has to be run once
install.packages("effsize")
library(effsize)  # load effsize package which contains Cohen's d
library(car)  # loads the car package-used later for recoding


summary(DEGREE) #IV
summary(as.numeric(CHILDS)) #DV

#updating data for DEGREE to count only high school and bachelor degrees
IV.degree<- NA #new independent variable
IV.degree <- ifelse(DEGREE=="HIGH SCHOOL", "High school",
                    ifelse (DEGREE=="BACHELOR", "Bachelor",
                            NA)) #copy high school and bachelor degree values from original to new and all else remain missing
table(IV.degree,DEGREE) # compare new trust variable with original.
table(IV.degree) #shows table of bachelors and hs degrees from all years
table(IV.degree, exclude=NULL) #excludes data to year 2016


#setting up DV 
cbind(table(CHILDS))#freq table in a single column
cbind(table(unclass(CHILDS))) #freq table of coded values in a single column; no children is valued at 1, 1 child is valued at 2, etc

DV.childs<-NA
DV.childs <- recode(as.numeric(CHILDS), "1=0; 2=1; 3=2; 4=3; 5=4; 6=5; 7=6; 8=7; 9=8;", as.factor.result = F) #sets no children(1) as 0, 1 child(2) as 1, 2(3) children as 2, etc


table(DV.childs)
table(CHILDS[YEAR==2016])

summary(as.numeric(DV.childs[YEAR==2016]))

table(IV.degree, DV.childs)


length(DEGREE) 
length(IV.degree)
length(DV.childs)
length(CHILDS)
length(DV.childs[!is.na(DV.childs)])


#calculations
summary(DV.childs) #min, max, mean of DV
summary(IV.degree)
sd(DV.childs, na.rm=T) # std.dev
length(DV.childs[!is.na(DV.childs)])
length(CHILDS) #returns n
length(IV.degree) #returns n

# Calculatig variances
var(DV.childs[DEGREE=="HIGH SCHOOL"], na.rm=T)  # variance of number of kids of those w/High school edu
var(DV.childs[IV.degree=="High school"], na.rm=T)  # confirm output matches above for recode success
var(DV.childs[IV.degree=="Bachelor"], na.rm=T)  # show variance of second group
var.test(DV.childs[IV.degree=="Bachelor"], DV.childs[IV.degree=="High school"])  #variance test

# Finding Mean of each group
mean(DV.childs[IV.degree=="High school"], na.rm=T)
mean(DV.childs[IV.degree=="Bachelor"], na.rm=T)
#Not going to expect any stat.sig difference or sizable effect.
#Finding St dev of each group
sd(DV.childs[IV.degree=="High school"], na.rm=T)
sd(DV.childs[IV.degree=="Bachelor"], na.rm=T)

# T-Test (independent sample)
t.test(DV.childs~IV.degree, pooled=T, paired=F, var.equal=T) 


# Cohen's d
cohen.d(DV.childs, IV.degree, na.rm=T, pooled=T, paired=F)
# d=-.2577=absolute value .2557, which is tiny.

