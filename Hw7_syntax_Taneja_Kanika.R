# INST 314 Homework 6 script answer key
# Shawn Janzen
# 3 April 2018

########################
# Question 1 to 4: No code

########################
# Question 5
# Cricket Chirps Vs. Temperature

# In the following data
# chirps.sec = chirps/sec for the striped ground cricket
# temp.f = temperature in degrees Fahrenheit
# Reference: The Song of Insects by Dr.G.W. Pierce, Harvard College Press
# http://college.cengage.com/mathematics/brase/understandable_statistics/7e/students/datasets/slr/frames/frame.html

cricket <- data.frame(
  chirps.sec = c(20.0,	16.0,	19.7,	18.4,	17.1,	15.5,	14.7,	17.1,	15.4,	16.2,	15.0,	17.2,	16.0,	17.0,	14.4),
  temp.f = c(88.6,	71.6, 93.3,	84.3,	80.6,	75.2,	69.7,	82.0,	69.4,	83.3,	79.6,	82.6,	80.6,	83.5,	76.3))


#check correlation 
cor.test(cricket$chirps.sec, cricket$temp.f)

#regression
summary(lm(cricket$chirps.sec~ cricket$temp.f))



######################################
# Question 6
#Ground Water Survey

#In the following data
#ph = pH of well water
#bicarbonate.ppm = Bicarbonate (parts per million) of well water
#The data is by water well from a random sample of wells in Northwest Texas.
#Reference: Union Carbide Technical Report K/UR-1
#http://college.cengage.com/mathematics/brase/understandable_statistics/7e/students/datasets/slr/frames/frame.html

wellwater <- data.frame(ph = c(7.6, 7.1, 8.2, 7.5, 7.4, 7.8, 7.3, 8.0, 7.1, 7.5, 8.1,	7.0, 7.3,	7.8, 7.3,	8.0,	8.5,	7.1,	8.2,	7.9, 7.6,	8.8, 7.2, 7.9, 8.1, 7.7,	8.4, 7.4, 7.3, 8.5, 7.8, 6.7, 7.1, 7.3),
                        bicarbonate.ppm = c(157,	174,	175,	188,	171,	143,	217,	190,	142,	190,	215,	199,	262,	105,	121,	81,	82,	210,	202, 155,	157, 147,	133, 53,	56,	113,	35,	125,	76,	48,	147,	117,	182,	87))

#regression
summary(lm(wellwater$ bicarbonate.ppm~ wellwater$ph))



######################################
# Question 7
#Pizza Franchise

#In the following data
#annual.franchise.fee = annual franchise fee ($1000)
#startup.cost = start up cost ($1000)
#for a pizza franchise
#Reference: Business Opportunity Handbook
#http://college.cengage.com/mathematics/brase/understandable_statistics/7e/students/datasets/slr/frames/frame.html

pizza <- data.frame(annual.franchise.fee = c(1000,	1125,	1087,	1070,	1100,	1150,	1250,	1150,	1100,	1350,	1275,	1375,	1175,	1200,	1175,	1300,	1260,	1330,	1325,	1200,	1225,	1090,	1075,	1080,	1080,	1180,	1225,	1175,	1250,	1250,	750,	1125,	700,	900,	900,	850),
                    startup.cost = c(1050,	1150,	1213,	1275,	1300,	1300,	1400,	1400,	1250,	1830,	1350,	1450,	1300,	1300,	1275,	1375,	1285,	1400,	1400,	1285,	1275,	1135,	1250,	1275,	1150,	1250,	1275,	1225,	1280,	1300,	1250,	1175,	1300,	1250,	1300,	1200))

#regression
summary(lm(pizza$startup.cost ~ pizza$annual.franchise.fee))



##############
data("USArrests")
# Murder: Murder arrests (per 100,000)
# Assault: Assault arrests (per 100,000)
# UrbanPop: Percent urban population
# Rape: Rape arrests (per 100,000)



# Modify the code below to create your plots


#IV is UrbanPop, DV is Murder
plot(USArrests$UrbanPop, USArrests$Murder,  # change IV & DV to your variables
     pch = 16, 
     cex = 1.3, 
     col = "blue", #optional: change color of your plotted dots
     main = "Murder vs UrbanPop",   # edit to  change your plot title
     xlab = "UrbanPop", # edit title of your x-axis
     ylab = "Murder") # edit title of your y-axis
abline(lm(USArrests$Murder ~ USArrests$UrbanPop, data = USArrests))  # change DV & IV to your variables.

#regression  
summary(lm(USArrests$Murder ~ USArrests$UrbanPop))

#IV UrbanPop, DV is Assault
plot(USArrests$UrbanPop, USArrests$Assault,  # change IV & DV to your variables
     pch = 16, 
     cex = 1.3, 
     col = "blue", #optional: change color of your plotted dots
     main = "Assault vs UrbanPop",   # edit to  change your plot title
     xlab = "UrbanPop", # edit title of your x-axis
     ylab = "Assault") # edit title of your y-axis
abline(lm(USArrests$Assault ~ USArrests$UrbanPop, data = USArrests))  # change DV & IV to your variables.

#regression
summary(lm(USArrests$Assault~ USArrests$ UrbanPop))

#IV UrbanPop, DV is Rape
plot(USArrests$UrbanPop, USArrests$Rape,  # change IV & DV to your variables
     pch = 16, 
     cex = 1.3, 
     col = "blue", #optional: change color of your plotted dots
     main = "Rape vs UrbanPop",   # edit to  change your plot title
     xlab = "UrbanPop", # edit title of your x-axis
     ylab = "Rape") # edit title of your y-axis
abline(lm(USArrests$Rape ~ USArrests$UrbanPop, data = USArrests))  # change DV & IV to your variables.

#regression
summary(lm(USArrests$Rape~ USArrests$ UrbanPop))



