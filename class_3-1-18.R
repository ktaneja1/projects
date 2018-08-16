install.packages("pwr") #install pwr package
library(pwr) #load the pwr package to use

#1 power analysis for t-test

#type: "one.sample" "two.sample"
#Alt: "two.sided" "greater" "less"
#d= effect size
pwr.t.test(n=NULL , d=.5 , sig.level=.01 , power=.8 , alternative="two.sided" , type="two.sample" )

# if 