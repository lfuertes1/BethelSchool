
#Exploring Data
#Create a histogram of the Financial Wellbeing Scale score
hist(financialWB$FWBscore)

library(ggplot2)
library(tidyr)
library(dplyr)

# Load data
financialWB <- read.csv("Bethel/Final Project/Data Sets/National Financial Well Being Survey/NFWBS_PUF_2016_data.csv")
FinData <- as.data.frame(financialWB)
class(FinData)

#Using ggplot
ggplot(FinData, aes(x=FWBscore)) +
  geom_histogram(binwidth = 3)

hist(financialWB$SAVINGSRANGES)

ggplot(FinData, aes(x=SAVINGSRANGES)) +
  geom_histogram(binwidth = 1)

#Recoding the SAVINGSRANGES variable
FinData$SAVINGSRANGESr <- NA
FinData$SAVINGSRANGESr[FinData$SAVINGSRANGES == 1]<-"0"
FinData$SAVINGSRANGESr[FinData$SAVINGSRANGES == 2]<-"1-99"
FinData$SAVINGSRANGESr[FinData$SAVINGSRANGES == 3]<-"100-999"
FinData$SAVINGSRANGESr[FinData$SAVINGSRANGES == 4]<-"1,000-4,999"
FinData$SAVINGSRANGESr[FinData$SAVINGSRANGES == 5]<-"5,000-19,999"
FinData$SAVINGSRANGESr[FinData$SAVINGSRANGES == 6]<-"20,000-74,999"
FinData$SAVINGSRANGESr[FinData$SAVINGSRANGES == 7]<-"75,000+"

head(FinData$SAVINGSRANGESr)

ggplot(FinData1, aes(x = factor(SAVINGSRANGESr))) +
  geom_histogram(binwidth = 1)


summary(FinData$SAVINGSRANGES)

#Remove records where savings ranges not specified and where FWBscore was < 0
FinData1<- FinData %>% filter(SAVINGSRANGES %in% c(1,2,3,4,5,6,7), FWBscore > 0)

# Checking the distribution of overall FWBscore
hist(FinData1$FWBscore)
# It looks very normal mean of 55.77

# Filtering the data frame by income brackets
bottom3rdincome <- filter(FinData, PPINCIMP %in% c(1,2,3))
bottom3rdincome <- filter(bottom3rdincome,SAVINGSRANGESr != 'NA')
bottom3rdincome <- filter(bottom3rdincome, FWBscore > 0)

top3rdincome <- filter(FinData, PPINCIMP %in% c(7,8,9))
top3rdincome <- filter(top3rdincome, SAVINGSRANGESr != 'NA')


#checking the distribution of FWBscore in the lower 3rd income bracket
hist(bottom3rdincome$FWBscore)
# looks very normal too with mean of 47.24
summary(bottom3rdincome$FWBscore)

#checking the distribution of FWBscore in the top 3rd income bracket
hist(top3rdincome$FWBscore)
# looks very normal too with mean of 60.91
summary(top3rdincome$FWBscore)


# Plot Two Histograms in one chart
hist(top3rdincome$FWBscore, col='red', main='Top Third Income Score and Bottom Third Income Score', xlab='x')
hist(bottom3rdincome$FWBscore, col='green', add=TRUE)

#add legend
legend('topleft', c('Top Third Income FWBscore ($75K+)', 'Bottom Third Income FWBscore (<$40K)'), fill=c('red', 'green'))




#Using ggplot
ggplot(FinData1, aes(x=FWBscore)) +
  geom_histogram(binwidth = 3)

# Summary statistics of the entire record set FWBscore and savingsranges of new 
# data frame where FWBscore is > 0

summary(FinData1$SAVINGSRANGES)

summary(FinData1$FWBscore)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#14.00   47.00   56.00   55.77   65.00   95.00

FinData1 %>% select('FWBscore') %>% filter(FWBscore < 0)

hist(FinData1$SAVINGSRANGES)

hist()



#Histogram of PPINCIMP - Participant household income
hist(FinData$PPINCIMP)



