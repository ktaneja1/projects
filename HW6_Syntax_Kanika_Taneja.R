#Homework 6

jewel <- read.csv(file.choose(), header=T)
library(summarytools)
library(car)  # for the levene test
library(sjstats)

#question 5a
# conduct anova DV~ IV
aov(jewel$Carat ~ jewel$Colour)
# save ANOVA as object
aov.jewels.color <- aov(jewel$Carat ~ jewel$Colour)
# show summary of ANOVA
summary(aov.jewels.color)
# show attributes of ANOVA
attributes(aov.jewels.color)
eta_sq(aov.jewels.color)

#tukey HSD test

TukeyHSD(aov(jewel$Carat ~ jewel$Colour, center= mean))
leveneTest(jewel$Carat ~ jewel$Colour, center= mean)

#5b
# conduct anova DV~ IV
aov(jewel$Carat ~ jewel$Clarity)
# save ANOVA as object
aov.jewels.clarity <- aov(jewel$Carat ~ jewel$Clarity)
# show summary of ANOVA
summary(aov.jewels.clarity)
# show attributes of ANOVA
attributes(aov.jewels.clarity)
eta_sq(aov.jewels.clarity)
#levene test bc p<.05
TukeyHSD(aov(jewel$Carat ~ jewel$Clarity, center= mean))
leveneTest(jewel$Carat ~ jewel$Clarity, center= mean)

#6
library(summarytools)
library(car)  # for the levene test
library(sjstats)
netflix <- read.csv(file.choose(), header=T)

netflix$rating3 <- ifelse(netflix$rating=="B", "other",
                          ifelse(netflix$rating=="B15", "other",
                                 ifelse(netflix$rating=="G", "other",
                                        ifelse(netflix$rating=="PG", "general",
                                               ifelse(netflix$rating=="TV-PG", "general",
                                                      ifelse(netflix$rating=="TV-14", "general",
                                                             ifelse(netflix$rating=="PG-13", "general",
                                                                    ifelse(netflix$rating=="R", "adult",
                                                                           ifelse(netflix$rating=="NC-17", "adult",
                                                                                  ifelse(netflix$rating=="TV-MA", "adult",
                                                                                         ifelse(netflix$rating=="UNRATED", "other",
                                                                                                ifelse(netflix$rating=="NOT RATED", "other",
                                                                                                       ifelse(netflix$rating=="Not specified", "other",
                                                                                                              NA )))))))))))))

# recode Netflix categories into 3 groups
netflix$rating3 <- ifelse(netflix$rating=="B", "other",
                          ifelse(netflix$rating=="B15", "other",
                                 ifelse(netflix$rating=="G", "other",
                                        ifelse(netflix$rating=="PG", "general",
                                               ifelse(netflix$rating=="TV-PG", "general",
                                                      ifelse(netflix$rating=="TV-14", "general",
                                                             ifelse(netflix$rating=="PG-13", "general",
                                                                    ifelse(netflix$rating=="R", "adult",
                                                                           ifelse(netflix$rating=="NC-17", "adult",
                                                                                  ifelse(netflix$rating=="TV-MA", "adult",
                                                                                         ifelse(netflix$rating=="UNRATED", "other",
                                                                                                ifelse(netflix$rating=="NOT RATED", "other",
                                                                                                       ifelse(netflix$rating=="Not specified", "other",
                                                                                                              NA )))))))))))))

# Recode Netflix year into decade periods using the cut function.
netflix$year4 = cut(as.numeric(netflix$year), breaks=c(1986, 1989, 1999, 2009, Inf))
netflix$year4 <- ordered(netflix$year4, labels = c("1980s", "1990s", "2000s", "2010s"))

# use the following line to subset the netflix data to only action movies
netflix_action <- netflix[which(netflix$genre=="Action"), ]

#summary stats of the 4 variables
netflix_action= netflix[which(netflix$genre=="Action"),]
summarytools::freq(netflix_action$score)
summarytools::freq(netflix_action$genre)
summarytools::freq(netflix_action$rating3)
summarytools::freq(netflix_action$year4)


#6B
qqnorm(netflix$score)
qqline(netflix$score)

#6c
netflix.aov=aov(netflix_action$score~ netflix_action$ year4 * netflix_action $rating3)
summary(netflix.aov)

 
#6d
TukeyHSD(aov(score ~ year4 * rating3, data=netflix_action), which="year4")
TukeyHSD(aov(score ~ year4 * rating3, data=netflix_action), which="rating3")
