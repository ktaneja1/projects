load(file.choose())
attach (gss7216)
library(car) #used for recoding
library(effsize)  # load effsize package which contains Cohen's d
library(summarytools)
install.packages("lsr")
library(lsr)



IV.agewedTest <- NA #new IV 

cbind(table(AGEWED)) #freq table in a single column
cbind(table(unclass(AGEWED))) #freq table of coded values in a single column; no children is valued at 1, 1 child is valued at 2, etc
# IV.agewedTest <- recode(as.numeric(AGEWED), "1=12; 2=13; 3=14; 4=15; 5=16; 6=17; 7=18; 8=19; 9=20;
#                        10=21; 11=22; 12=23; 13=24; 14=25; 15=26; 16=27; 17=28; 18=29;
#                        19=30; 20=31; 21=32; 22=33; 23=34; 24=35; 25=36; 26=37; 27=38;
#                        28=39; 29=40; 30=41; 31=42; 32=43; 33=44; 34=45; 35=46; 36=47;
#                        37=48; 38=49; 39=50; 40=51; 41=52; 42=53; 43=54; 44=55; 45=56;
#                        46=57; 47=58; 48=59; 49=60; 50=61; 51=62; 52=63; 53=65; 54=68;
#                        55=70; 56=73; 57=90;", as.factor.result = F) #recoding, later discovered a more efficient way where this would not be necessary


agewed2= as.numeric (AGEWED)+11
agewed2= as.numeric(IV.agewedTest) # I later found out that this could be simplier, to do agewed2= as.numeric (AGEWED)+11
agebreak= cut(agewed2, breaks=c(10, 19, 29, 39, 49, 59, 69, Inf))

table (AGEWED, agebreak) #check if recoding is correct

table(agebreak)
#recoding for dependent variable to only include yes and no responses
DV.evdivTest <-NA
DV.evdivTest <- ifelse(EVDIV=="YES", "Yes",
                       ifelse (EVDIV=="NO", "No",
                               NA))

table(DV.evdivTest, EVDIV)
table(agebreak, DV.evdivTest) #table of recoded variables
table(DV.evdivTest,exclude = NULL)


summary(as.numeric(agebreak[YEAR==1988 | YEAR==1994]))

table(agebreak, DV.evdivTest)
length(AGEWED)
length(agebreak[!is.na(agebreak)]) #gives n excluding Na's
length(EVDIV)
length(DV.evdivTest)
length(DV.evdivTest[!is.na(DV.evdivTest)]) #gives n excluding NA's






 # gss_subset= subset(gss7216, gss7216$EVDIV== "YES" | gss7216$EVDIV=="NO")
# table(gss_subset$agebreak)
table(DV.evdivTest, agebreak)

tab.marriage= table(DV.evdivTest, agebreak)
chisq.test(tab.marriage)
#This is giving NaN values for X^2 and NA for p-value because some cells have small values.
#To combat this error I will recode the age groups
agebreak_again= cut(agewed2, breaks=c(10, 19, 29,39,49))


table(agebreak_again, DV.evdivTest)
tab.marriage_cut= table(DV.evdivTest, agebreak_again)
chisq.test(tab.marriage_cut)  #returns chi sq values
Test= chisq.test(tab.marriage_cut)
Test$statistic
Test$p.value
Test$observed
Test$expected


cramersV(agebreak_again, DV.evdivTest)  #.1795123, this is a small effect size.









