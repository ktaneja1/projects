
#Script for Problem 2
attach(gss) #don't have to type dataframe name
table (HAPPY)  #frequency table
length(HAPPY)  	# n, variable sample size
table(HAPPY)/ length(HAPPY)	# proportion table
cbind(table(HAPPY)/(length(HAPPY))) # proportion table output in a column
# one sample proportion t-test
#T-test for Very happy
binom.test(806, 2867,           # frequency of response, n
           .281,                # Pu, probability of success
           alternative="two.sided",    # two tail test
           conf.level=.95)   # 1 - alpha

#T-test for pretty happy
binom.test(1601, 2867,           # frequency of response, n
           .558,                # Pu, probability of success
           alternative="two.sided",    # two tail test
           conf.level=.95)   # 1 - alpha

#T-test for not too happy
binom.test(452, 2867,           # frequency of response, n
           .158,                # Pu, probability of success
           alternative="two.sided",    # two tail test
           conf.level=.95)   # 1 - alpha


#Script for Problem 3

# Create an optimism scale variable using LOTR1 to LOTR6

# Do you agree or disagree that...

# 1) In uncertain times, I usually expect the best.
LOTR1.r <- as.numeric(LOTR1)  # convert
table(LOTR1, LOTR1.r)         # confirm recode

# 2) If something can go wrong for me, it will.  (need to reverse code)
LOTR2.r <- as.numeric(LOTR2)  # convert
LOTR2.r <- 6 - LOTR2.r        # reverse code
table(LOTR2, LOTR2.r)         # confirm recode

# 3) I'm always optimistic about my future.
LOTR3.r <- as.numeric(LOTR3)  # convert
table(LOTR3, LOTR3.r)         # confirm recode

# 4)I hardly ever expect things to go my way.  (need to reverse code)
LOTR4.r <- as.numeric(LOTR4)  # convert
LOTR4.r <- 6 - LOTR4.r        # reverse code
table(LOTR4, LOTR4.r)         # confirm recode

# 5) I rarely count on good things happening to me. (need to reverse code)
LOTR5.r <- as.numeric(LOTR5)  # convert
LOTR5.r <- 6 - LOTR5.r        # reverse code
table(LOTR5, LOTR5.r)         # confirm recode

# 6)  Overall, I expect more good things to happen to me than bad.
LOTR6.r <- as.numeric(LOTR6)  # convert
table(LOTR6, LOTR6.r)         # confirm recode
# create & explore the LOTR scale variable
LOTR.scale <- (LOTR1.r + LOTR2.r + LOTR3.r + LOTR4.r + LOTR5.r + LOTR6.r) -5
summary(LOTR.scale)
sd(LOTR.scale, na.rm=T) #returns standard deviation
hist(LOTR.scale) #creates histogram
#binom.test(????, 2867,           # frequency of response, n
#           ???,                # Pu, probability of success
#           alternative="?????",    # two tail test
#           conf.level=.99)   # 1 - alpha


#Question 3d

LOTR.xbar <- mean(LOTR.scale, na.rm=T)
LOTR.s <-  sd(LOTR.scale, na.rm=T)
LOTR.n <-  length(LOTR.scale)

#finding Confidence interval
LOTR.scale <- na.omit(LOTR.scale)
CI(LOTR.scale, .99)









