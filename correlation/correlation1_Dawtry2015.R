# Load packages
library(tidyverse)
library(psych)

# read data
df <-  read_csv("Dawtry Sutton and Sibley 2015 Study 1a.csv")

# 1. Open the data file (called Dawtry Sutton and Sibley 2015 Study 1a). Explore the data file. Note,
# you will not analyze all of these variables. Try to find the variables that are relevant to the study
# description above.
View(df)

# 2. In order to conduct the analyses properly, you will first need to compute
# two new variables: 
# a. You should first create a score that captures
# participants' perceptions that the current system is fair and satisfactory. To
# do this, use mutate() to generate  the mean for the items fairness and 
# satisfaction, naming this new variable FAIRNESS_AND_SATISFACTION. 
# b. Next, you should create a score that captures participants' support for
# redistribution. The researchers asked participants four questions in total,
# two asked about their support for redistribution, and two asked about their
# opposition to redistribution. To create a single score that reflect
# participants overall view toward redistribution, we first need to recode the
# two items that assess opposition to redistribution. Reverse score redist2 and
# redist4, so that 6 = 1, 5 = 2, 4 = 3, 3 = 4, 2 = 5, 1 = 6, using the mutate
# function, remember that a likert item can be reverse scored by subtracting the
# item score from one more than the maximum possible value on that scale. Name
# the recoded variables redist2_recode and redist4_recode. Now, generate the
# mean for the items redist1, redist2_recode, redist3, redist4_recode, naming
# this new variable SUPPORT_FOR_REDISTRIBUTION

df <- mutate(df, 
             FAIRNESS_AND_SATISFACTION = (fairness + satisfaction)/2,
             redist2_recode = 7 - redist2,
             redist4_recode = 7 - redist4,
             SUPPORT_FOR_REDISTRIBUTION = (redist1 + redist2_recode + redist3 + redist4_recode)/4)

# 3. You should next calculate the means and standard deviations for the key 
# variables in the study: Household Income, Social-Circle Income, Population 
# Mean Income, Social-Circle Inequality, Population Inequality, Fairness and 
# Satisfaction, Support for Redistribution, and Political Preference. The
# describe() function in the psych package will be useful
describe(df)

# 4. Run correlation analyses for all of the key variables in the study (see
# list #3 above). It might be easiest to subset your dataset down to just these variables. 
df1 <-  df %>% select(Household_Income, 
                      Social_Circle_Mean_Income, 
                      Population_Mean_Income, 
                      Social_Circle_Inequality_Gini_Index,
                      Population_Inequality_Gini_Index, 
                      FAIRNESS_AND_SATISFACTION, 
                      SUPPORT_FOR_REDISTRIBUTION,
                      Household_Income)

# 5. Prepare a correlation matrix that includes all of the relevant study
# variables. Make sure to follow APA-style guidelines. The corr.test() function
# in the psych package will give you a correlation matrix with n's and p values.
corr.test(df1)

