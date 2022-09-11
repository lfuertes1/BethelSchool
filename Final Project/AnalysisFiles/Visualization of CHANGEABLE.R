# Visualization: Treemap of "Belief that ability to manage money is NOT changeable" variable

install.packages("treemap")
install.packages("scales")

library(treemap)
library(scales)
library(dplyr)

FinData <- read.csv("Bethel/Final Project/Data Sets/National Financial Well Being Survey/NFWBS_PUF_2016_data.csv")

summary(FinData$CHANGEABLE)

FinDataTree <- FinData %>% select(CHANGEABLE) %>% filter(CHANGEABLE > 0)

summary(FinDataTree)

FinDataTree1 <- FinDataTree %>% count(CHANGEABLE)

summary(FinDataTree1)

# Recode

FinDataTree1$CHANGEABLEr <- NA
FinDataTree1$CHANGEABLEr[FinDataTree1$CHANGEABLE == 1]<-"Strongly disagree"
FinDataTree1$CHANGEABLEr[FinDataTree1$CHANGEABLE == 2]<-"Disagree"
FinDataTree1$CHANGEABLEr[FinDataTree1$CHANGEABLE == 3]<-"Somewhat disagree"
FinDataTree1$CHANGEABLEr[FinDataTree1$CHANGEABLE == 4]<-"Neither agree nor disagree"
FinDataTree1$CHANGEABLEr[FinDataTree1$CHANGEABLE == 5]<-"Somewhat agree"
FinDataTree1$CHANGEABLEr[FinDataTree1$CHANGEABLE == 6]<-"Agree"
FinDataTree1$CHANGEABLEr[FinDataTree1$CHANGEABLE == 7]<-"Strongly agree"

png(filename="tree1.png",width=800, height=800)
treemap(FinDataTree1,index=c("CHANGEABLEr"), vSize="n", type="index", title="Belief that ability to manage money is NOT changeable")
