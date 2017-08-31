# Load packages
library(tidyverse)
library(psych)

# 1. Open the data file (called Atir Rosenzweig Dunning 2015 Study 1b).
# read data
df <-  read_csv("Atir Rosenzweig Dunning 2015 Study 1b.CSV")
 
# 2. First, calculate means and standard deviations for overclaiming.
describe(df$overclaiming_proportion)
 
# 3. You next want to examine the relationship between self-perceived knowledge
# and overclaiming. You also want to take into account the accuracy with which
# participants responded during the overclaiming task (that is the ability of
# people to distinguish between the 12 real terms and the 3 fake terms). Conduct
# an analysis that uses both self-perceived knowledge and accuracy to predict
# overclaiming.
mod <- lm(overclaiming_proportion ~ self_perceived_knowledge + accuracy, data=df) 
summary(mod)

# 4. You next want to determine whether there is an order effect (based on
# whether participants completed the self-perceived knowledge measure first, or
# the overclaiming task first. Compare the mean level of overclaiming based on
# the order of the tasks.
t.test(overclaiming_proportion~order_of_tasks, data=df, var.equal=TRUE)
 
# 5. If you found a significant difference in overclaiming in the analysis above
# (#4), re-perform the analysis from #3 to check to see if the relationship 
# between self-perceived knowledge and overclaiming changes, when taking into 
# account the order of the tasks. To do this, split the file based on order
# condition, before re-performing the analyses from #3. After you have performed
# the analyses.
ord1 <-  filter(df, order_of_tasks==1)
ord2 <-  filter(df, order_of_tasks==2)
mod_or1 <- lm(overclaiming_proportion ~ self_perceived_knowledge + accuracy, data=ord1)
mod_or2 <- lm(overclaiming_proportion ~ self_perceived_knowledge + accuracy, data=ord2)

summary(mod_or1)
summary(mod_or2)

# 6. You next want to determine if the self-perceived knowledge still predicts 
# overclaiming while accounting for the variance due to genuine expertise, as 
# measured by the FINRA. First, find the mean and standard deviation for scores 
# on the FINRA. Then, re-perform the analysis from #3, but this time include 
# scores on the FINRA as an additional predictor variable.
describe(df$FINRA_score)
mod1 <- lm(overclaiming_proportion ~ self_perceived_knowledge + accuracy + FINRA_score, data=df)
summary(mod1) 
# 7. Prepare an APA-style results section for the analyses you completed.