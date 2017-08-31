# Load packages
library(tidyverse)
library(lm.beta)

# read data
df <-  read_csv("Tworek and Cimpian 2016 Study 1.CSV")

# 1. Open the data file (called Tworek and Cimpian Study 1). Explore the data
# file. Note, you will not analyze all of these variables. Try to find the
# variables that are relevant to the study description above.
View(df)
names(df)

# 2. This data file includes some participants who were excluded from the data
# analysis because they live outside the United States, or because they failed
# an attention check. Using the filter() function, use the excluded variable
# (labeled Excluded = 0). Check the data file to ensure that excluded
# participants are not selected for analysis.
df1 <-  filter(df, excluded==0)

# 3. Perform an analysis to determine whether people's inherence bias is
# associated with their tendency to believe the status quo ought to be
# (Ought_Score).
cor.test(df1$Inherence_Bias, df1$Ought_Score)

# 4. Now, you want to show that this relationship is robust. In other words, the
# relationship between a person's inherence bias and their ought. Perform a
# multiple regression with ought_score as the outcome variable and inherence
# bias, education level, Raven's Progressive matrix score, conservatism, and
# belief in a just world as predictor variables. Interpret the results.
mod1 <- lm(Ought_Score ~ Inherence_Bias + 
             educ +
             RavensProgressiveMatrix_sum + 
             conserv + 
             Belief_in_Just_World 
             , data=df1)
lm.beta(mod1)
summary(mod1)

# 5. Prepare an APA-style results section to describe each of the analyses conducted above.

# 6. Create a table to depict the results of the regression analysis.