# Question 1 
# How does PAREDUC Influence current income PPINCIMP?

# Using Independent Chi-Square analysis

# Load data
FinData <- read.csv("Bethel/Final Project/Data Sets/National Financial Well Being Survey/NFWBS_PUF_2016_data.csv")

class(FinData)

FinData

# Independent Chi-Square

# Loading the required library
library(gmodels)
library(scales)



# Data Wrangling
FinData2 <- FinData %>% filter(PAREDUC > 0)
FinData2 <- FinData %>% filter(PPINCIMP > 0)

## Verify values
summary(FinData2$PAREDUC)
summary(FinData2$PPINCIMP)

## Recoding for ease of interpretation
FinData2$PAREDUCr <- NA
FinData2$PAREDUCr[FinData2$PAREDUC == 1]<-"High school/GED or less"
FinData2$PAREDUCr[FinData2$PAREDUC == 2]<-"High school/GED or less"
FinData2$PAREDUCr[FinData2$PAREDUC == 3]<-"Some college/Associate"
FinData2$PAREDUCr[FinData2$PAREDUC == 4]<-"BA/BS degree"
FinData2$PAREDUCr[FinData2$PAREDUC == 5]<-"Post grad degree"


FinData2$PPINCIMPr <- NA
FinData2$PPINCIMPr[FinData2$PPINCIMP == 1]<-"$0 to $29,999"
FinData2$PPINCIMPr[FinData2$PPINCIMP == 2]<-"$0 to $29,999"
FinData2$PPINCIMPr[FinData2$PPINCIMP == 3]<-"$30,000 to $39,999"
FinData2$PPINCIMPr[FinData2$PPINCIMP == 4]<-"$40,000 to $49,999"
FinData2$PPINCIMPr[FinData2$PPINCIMP == 5]<-"$50,000 to $59,999"
FinData2$PPINCIMPr[FinData2$PPINCIMP == 6]<-"$60,000 to $74,999"
FinData2$PPINCIMPr[FinData2$PPINCIMP == 7]<-"$75,000 to $99,999"
FinData2$PPINCIMPr[FinData2$PPINCIMP == 8]<-"$100,000 to $149,999"
FinData2$PPINCIMPr[FinData2$PPINCIMP == 8]<-"$150,000 or more"

FinData2EdInc <- FinData2 %>% select(PAREDUCr,PPINCIMPr)

FinData2EdInc <- na.omit(FinData2EdInc)

# Rename variables

FinData2EdInc <- rename(FinData2EdInc, "ParentEduc" = PAREDUCr)

# Visualization

## stacked bar chart
ggplot(FinData2EdInc, 
       aes(x = PPINCIMPr, 
           fill = ParentEduc)) + 
  geom_bar(position = "fill") +
  scale_y_continuous(breaks = seq(0, 1, .2), label = percent) +
  scale_fill_brewer(palette = "Set2") +
  labs(y = "Percent", 
       x = "Participant's Current Income",
       title = "Participant's Income by Parent's Highest Education Level") +
  theme_minimal()

##Improved visualization

plotdata <- FinData2EdInc %>%
  group_by(PPINCIMPr, ParentEduc) %>%
  summarize(n = n()) %>% 
  mutate(pct = n/sum(n),
         lbl = scales::percent(pct))
plotdata

ggplot(plotdata, 
       aes(x = PPINCIMPr, 
           fill = ParentEduc,
           y = pct)) + 
  geom_bar(stat = "identity",
           position = "fill") +
  scale_y_continuous(breaks = seq(0, 1, .2), 
                     label = percent) +
  geom_text(aes(label = lbl), 
            size = 3, 
            position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Set2") +
  labs(y = "Percent", 
       fill = "Parent's Highest Education",
       x = "Participant's Current Income",
       title = "Participant's Income by Parent's Highest Education Level") +
  theme_minimal()


# Testing assumptions - 
CrossTable(FinData2EdInc$ParentEduc,FinData2EdInc$PPINCIMPr, fisher=TRUE,chisq=TRUE,expected=TRUE,sresid = TRUE,
           format="SPSS")

# Statistics for All Table Factors


#Pearson's Chi-squared test 
#------------------------------------------------------------
#  Chi^2 =  457.0155     d.f. =  18     p =  1.100142e-85 


# Check the assumptions of expected frequencies
# Need to have more than 5 on the second row of each cell

# Interpret the results:

# Based on the p value being less than .05, PAREDUC influences the PPINCIMPr - participant's income.

# Post Hocs Interpretation below - absolute value of 2 or more is significantly different than the expected value.

# For parents with high school or less, more participants earn $30k or less than expected and less
# participants earn more than $150k than expected.

# For parents with some college/associates degree, less participants earn $30K or less than expected but just barely. 
# 
# For parents with Bachelors degree, less participants earn $30k or less than expected and more 
# earn $150K than expected.
# 
# For parents with a post grad degree, less participants earn $30K than expected and 
# more participants earn $150K or more than expected.

#***************************************************************************************************#


# Part Two
# Is there a difference in PPINCIMP based on whether the participant sets FINGOALS or not?

# Using One-Way Between Subject ANOVA

# Load the required libraries
library("dplyr")
library("rcompanion")
library("car")
library(tidyr)

# Data wrangling 
# Remove the -1 - refused to answer records
FinData3 <- FinData %>% filter(FINGOALS %in% c(0,1))


# Recoding the FINGOALS variable
FinData3$FINGOALSr <- NA
FinData3$FINGOALSr[FinData3$FINGOALS == 1]<-"Yes"
FinData3$FINGOALSr[FinData3$FINGOALS == 0]<-"No"

# New data frame with only the variables needed
FinData4 <- FinData3 %>% select(FINGOALSr,PPINCIMP)

# Remove missing values and n/a
FinData4 <- na.omit(FinData4)

summary(FinData4$PPINCIMP)

# Make PPINCIMP numeric as the dependent variable

FinData4$PPINCIMP <- as.numeric(FinData4$PPINCIMP)

str(FinData4)

# Test assumptions

# 1. Normality

    plotNormalHistogram(FinData4$PPINCIMP)
    
    # The data is negatively skewed - Try squaring the data first
    FinData4$PPINCIMP_SQ<- FinData4$PPINCIMP * FinData4$PPINCIMP
    
    plotNormalHistogram(FinData4$PPINCIMP_SQ)
    
    # A little better
    # Will cube the data
    
    FinData4$PPINCIMP_CUBE<- FinData4$PPINCIMP ^ 3
    
    plotNormalHistogram(FinData4$PPINCIMP_CUBE)
    
    # the data is somewhat better
    # Will use the Tukeys Ladder now
    # Need to take the first 5000 samples
    FinData5 <- FinData4[1:5000,]
    
    FinData5$PPINCIMP_TUK <- transformTukey(FinData5$PPINCIMP,plotit = FALSE)
    
    plotNormalHistogram(FinData5$PPINCIMP_TUK)
    # The data is not all normal, it's still a bit negatively skewed

# 2. Homogeneity of Variance
    
    # Barttlet's Test
    bartlett.test(PPINCIMP_TUK ~ FINGOALSr, data = FinData5)
    
    # Bartlett test of homogeneity of variances
    
    # data:  PPINCIMP_TUK by FINGOALSr
    # Bartlett's K-squared = 5.7195, df = 1, p-value = 0.01678
    
    # Due to the p value being < .05 it means this data violated the assumption
    # of homogeneity of variance
    
    # Fligner's Test
    fligner.test(PPINCIMP_TUK ~ FINGOALSr, data = FinData5)
    
    # Fligner-Killeen test of homogeneity of variances
    
    # data:  PPINCIMP_TUK by FINGOALSr
    # Fligner-Killeen:med chi-squared = 0.63971, df = 1, p-value = 0.42
    
    # the p value on this test is > .05 and therefore meets the homogeneity of variance
    # assumption and it's better suited for the data which that normal

# Calculating the ANOVA
    
    FinData4ANOVA <- aov(FinData5$PPINCIMP_TUK ~ FinData5$FINGOALSr)
    
    summary(FinData4ANOVA)
    
    #                      Df Sum Sq Mean Sq F value Pr(>F)    
    #FinData5$FINGOALSr    1   9175    9175   154.6 <2e-16 ***
    # Residuals          4998 296611      59                   
    # ---
    #  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    
    # Perform Post Hocs no adjustment
    pairwise.t.test(FinData5$PPINCIMP_TUK, FinData5$FINGOALSr, p.adjust="none")
    
    # Pairwise comparisons using t tests with pooled SD 
    
    # data:  FinData5$PPINCIMP_TUK and FinData5$FINGOALSr 
    
    # No    
    # Yes <2e-16
    
    # P value adjustment method: none 
    
    # Calculate Post Hocs with adjustment
    pairwise.t.test(FinData5$PPINCIMP_TUK, FinData5$FINGOALSr, p.adjust="bonferroni")
    
    # Pairwise comparisons using t tests with pooled SD 
    
    # :  FinData5$PPINCIMP_TUK and FinData5$FINGOALSr 
    
    # No    
    # Yes <2e-16
    
    # P value adjustment method: bonferroni 
    
    #  Determine Means and draw conclusions
    FinData5Means <- FinData5 %>% group_by(FINGOALSr) %>% summarize(MEAN = mean(PPINCIMP))
    
    FinData5Means
    
    # # A tibble: 2 x 2
    # FINGOALSr  MEAN
    # <chr>     <dbl>
    # 1 No         5.03 - $50,000 to $59,999
    # 2 Yes        6.01 - $60,000 to $74,999
    
    
    # Visualize the data
    
    FinData5Means$MEAN <- as.integer(FinData5Means$MEAN)
    # Create a subset to plot it no -1 records
    
    # Recode the Mean variable for visualization
    FinData5Means$MEANr <- NA
    FinData5Means$MEANr[FinData5Means$MEAN == 5]<-"$50,000 to $59,999"
    FinData5Means$MEANr[FinData5Means$MEAN == 6]<-"$60,000 to $74,999"
    
    class(FinData5Means)
    
    ## Additional wrangling
    
    FinData5Means1 <- FinData5Means %>% select(FINGOALSr,MEANr)
    
    FinData5Means1 <- rename(FinData5Means1, "AverageIncome" = MEANr,"HasFinancialGoals" = FINGOALSr)
    
    
    # Visualization
    
    ## Use the FinData2 data frame as it has the recoded participan't current income 
    ## Recode the FINGOALS varble
    FinData2$FINGOALSr <- NA
    FinData2$FINGOALSr[FinData2$FINGOALS == 1]<-"Yes"
    FinData2$FINGOALSr[FinData2$FINGOALS == 0]<-"No"
    
    FinData2noNA <- na.omit(FinData2)
    
    plotdata2 <- FinData2noNA %>%
      group_by(PPINCIMPr, FINGOALSr) %>%
      summarize(n = n()) %>% 
      mutate(pct = n/sum(n),
             lbl = scales::percent(pct))
   
    ggplot(plotdata2, 
           aes(x = PPINCIMPr, 
               fill = FINGOALSr,
               y = pct)) + 
      geom_bar(stat = "identity",
               position = "fill") +
      scale_y_continuous(breaks = seq(0, 1, .2), 
                         label = percent) +
      geom_text(aes(label = lbl), 
                size = 3, 
                position = position_stack(vjust = 0.5)) +
      scale_fill_brewer(palette = "Set2") +
      labs(y = "Percent", 
           fill = "Participant Has/Recently Had Financial Goal",
           x = "Participant's Current Income",
           title = "Participant's Income by Setting Financial Goals") +
      theme_minimal()
    
    
    
    








