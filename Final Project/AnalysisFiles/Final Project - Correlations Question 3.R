# How does FS1_7 "I know how to make myself save" correlate to SAVINGSRANGES or how much money do you have in savings today?

# Correlations test


# Loading the required libraries

library(ggplot2)
library(tidyr)
library(dplyr)

# Loading the data

FinData <- read.csv("Bethel/Final Project/Data Sets/National Financial Well Being Survey/NFWBS_PUF_2016_data.csv")

head(FinData)

# Data Wrangling
# Create a subset with the two variab less needed
FinData1 <- FinData %>% select(FS1_7,SAVINGSRANGES)

# Filter out the answers that are negative or that don't have meaninful data 
FinData1 <- FinData1 %>% filter(FS1_7 > 0)
FinData1 <- FinData1 %>% filter(SAVINGSRANGES %in% c(1,2,3,4,5,6,7))

# Create a scatter plot of the data to visualize the correlation

Viz <- ggplot(FinData1, aes(x=SAVINGSRANGES, y = FS1_7))
Viz + geom_point() + geom_smooth(method=lm) + ggtitle("Correlation between Savings ranges and the question on whether the participant can make him or herself save money") +
  xlab("Savings: 1 <- $0, 2 <-$1-99, 3 <-$100-999, 4 <-$1,000-4,999, 5 <-5,000-19,999,6 <-$20,000-74,999, 7 <- $75,000 +")+
  ylab("I know how to make myself save: 1 <- Not at all, 5 <- Completely")

# Not a good visual as all the points fall into intersections. However, when adding the linear model
# we see that there's a positive correlation 

# Calculating the correlation

# Checking for normality - The data is somewhat normal
hist(FinData1$FS1_7)
hist(FinData1$SAVINGSRANGES)


cor.test(FinData1$SAVINGSRANGES, FinData1$FS1_7 , method="pearson", use = "complete.obs")

# Pearson's product-moment correlation
# 
# data:  FinData1$FS1_7 and FinData1$SAVINGSRANGES
# t = 35.264, df = 5207, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.4168814 0.4607317
# sample estimates:
#      cor 
# 0.439068 

#**********************************************************************#
# Conclusion:

# Based on the correlation coefficient of 0.44, these two variables 
# have a moderate positive correlation

#**********************************************************************#



# How does the perceived Financial Skill correlate to the Financial Wellness scale score?

# Correlations test

# Data Wrangling
# Create a subset with the two variable less needed
FinDataCorr <- FinData %>% select(FSscore,FWBscore)

# Remove any record for participants that refused to answer (-1)
FinDataCorr <- FinDataCorr %>% filter(FSscore > 0)
FinDataCorr <- FinDataCorr %>% filter(FWBscore > 0)

# Create a scatter plot of the data to visualize the correlation

Viz2 <- ggplot(FinDataCorr, aes(x=FSscore, y = FWBscore))
Viz2 + geom_point() + geom_smooth(method=lm) + ggtitle("Financial Skill Score vs Financial Wellbeing Scale Score") +
  xlab("Financial Skill Score")+
  ylab("Financial Wellbeing Scale Score")

# There is a positive correlation between these two variables

# Calculating the correlation

# Checking for normality - The data is somewhat normal
hist(FinDataCorr$FSscore)
hist(FinDataCorr$FWBscore)

# The data is normally distributed

cor.test(FinDataCorr$FSscore, FinDataCorr$FWBscore , method="pearson", use = "complete.obs")

# Pearson's product-moment correlation
# 
# data:  FinDataCorr$FSscore and FinDataCorr$FWBscore
# t = 45.15, df = 6383, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.4731799 0.5103684
# sample estimates:
#       cor 
# 0.4919985 


#**********************************************************************#
# Conclusion:

# Based on the correlation coefficient of 0.50, these two variables 
# have a moderate positive correlation

#**********************************************************************#