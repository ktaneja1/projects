#Question 1B
# One way to create table data in R is to manually input the values.  Here, we will create each gender as an object with the values (in order) reflecting the frequencies for ice cream preference.
men = c(100, 120, 60)
women = c(350, 200, 90)

# combining the row vectors in matrices, then converting the matrix into a data frame
ice.cream.survey = as.data.frame(rbind(men, women))

# assigning column names to this data frame
# each column name corresponds to an ice cream flavor, which matches the order you used on the rows.
names(ice.cream.survey) = c('Strawberry', 'Vanilla', 'Chocolate')

# view your data frame as a table
ice.cream.survey

# perform Chi-Squre test
chisq.test(ice.cream.survey)

#sum to find n
sum(men)
sum(women)

#Question 4
USA= c(65, 32, 1, 98)
Europe= c (75, 22, 1, 98)
Asia=c (33, 34, 30, 97)
Total= c( 173, 88, 32, 293)

#combining into matrix
pronunciation.data = as.data.frame(rbind(USA, Europe, Asia, Total))
#renaming the columns
names(pronunciation.data)= c('Hard g','Soft g', 'All Letters' , 'Total') 

# view your data frame as a table
pronunciation.data
# perform Chi-Squre test
chisq.test(pronunciation.data)


#question 5
install.packages("summarytools")
library(summarytools)

load(file.choose()) # find & open the 2016 GSS dataset

class(gss$AGE)  # examine how R 'sees' the AGE variable

# 'permanently' convert age from factor to numeric as new variable
age.r = as.numeric(gss$AGE)+17  # add 17, because when converted, age 18 = 1, so add 17 to make 18 = 18, 19=19, etc.

# report summary stats for age by gender
summary(age.r[gss$SEX=="MALE"])
summary(age.r[gss$SEX=="FEMALE"])

# create age groups with each age group = 5 years, plus low and high age caps
age9 = cut(age.r, breaks=c(18,25, 30, 35, 40, 45, 50, 55, 60, 65, Inf)) # age 9 since there are 9 groups
table(gss$AGE, age9)  # confirm recode
summarytools::freq(age9[gss$SEX=="MALE"])  # check summary stats for men
summarytools::freq(age9[gss$SEX=="FEMALE"])  # check summary stats for women

# take a look at the DV: pass good job opportunities for family benefit
# separate the DV by gender
summarytools::freq(gss$FAMORJOB[gss$SEX=="MALE"])  # check summary stats for men
summarytools::freq(gss$FAMORJOB[gss$SEX=="FEMALE"])  # check summary stats for women

# use the following code to create bivariate tables & compute Chi Square for each gender 
addmargins(table(gss$FAMORJOB[gss$SEX=="MALE"], age9[gss$SEX=="MALE"])) # bivariate table with marginals for men
tab.job.men = table(gss$FAMORJOB[gss$SEX=="MALE"], age9[gss$SEX=="MALE"]) # store table above as an object
  
  addmargins(table(gss$FAMORJOB[gss$SEX=="FEMALE"], age9[gss$SEX=="FEMALE"])) # bivariate table with marginals for women
tab.job.women = table(gss$FAMORJOB[gss$SEX=="FEMALE"], age9[gss$SEX=="FEMALE"])  #store table above as an object

chisq.test(tab.job.men) # Chi-Square for job x age groups for men
chisq.test(tab.job.women)  # Chi-Square for job x age groups for women
# use the following code to create bivariate tables & compute Chi Square for each gender 
sum(tab.job.men)
sum(tab.job.women)


# Start with a summary of the previously created age variable
summary(age.r)

age4 = cut(age.r, breaks=c(18, 34, 49, 62, Inf)) # modify this code from 9 groups to 4 groups using the age quartiles

table(gss$AGE, age4)  # confirm recode
summarytools::freq(age4)  # check summary stats

table(gss$AGE, age4)  # confirm recode
summarytools::freq(age4[gss$SEX=="MALE"])  # check summary stats for men
summarytools::freq(age4[gss$SEX=="FEMALE"])

#Question 5d

# use the following code to create bivariate tables & compute Chi Square for each gender 
addmargins(table(gss$FAMORJOB[gss$SEX=="MALE"], age4[gss$SEX=="MALE"])) # bivariate table with marginals for men
tab.job.men = table(gss$FAMORJOB[gss$SEX=="MALE"], age4[gss$SEX=="MALE"]) # store table above as an object
  
addmargins(table(gss$FAMORJOB[gss$SEX=="FEMALE"], age4[gss$SEX=="FEMALE"])) # bivariate table with marginals for women
tab.job.women = table(gss$FAMORJOB[gss$SEX=="FEMALE"], age4[gss$SEX=="FEMALE"])  #store table above as an object

chisq.test(tab.job.men) # Chi-Square for job x age groups for men
chisq.test(tab.job.women)  # Chi-Square for job x age groups for women

# %
round(addmargins(prop.table(table(gss$FAMORJOB[gss$SEX=="MALE"], age4[gss$SEX=="MALE"]),2)),3)
round(addmargins(prop.table(table(gss$FAMORJOB[gss$SEX=="FEMALE"], age4[gss$SEX=="FEMALE"]),2)),3)




