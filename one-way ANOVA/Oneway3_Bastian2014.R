# Load packages
library(tidyverse)
library(afex)
library(Rmisc)

# 1. Open the data file (called Bastian Jetten and Ferris Experiment 1). Explore
# the data file. Note, you will not analyze all of these variables. Try to find
# the variables that are relevant to the study description above.
df <-  read_csv("Bastian Jetten and Ferris 2014 Experiment 1.csv") 


df <- df %>% mutate(CONDITION = factor(case_when(CONDITION == 0 ~ "Control",
                                                 CONDITION == 1 ~ "Pain")), 
                    ID = 1:NROW(df))

# 2. You first run an analysis to test whether the pain manipulation was
# successful. Conduct a t-test to compare participants in the pain condition vs.
# the control condition for the pain intensity and pain unpleasantness
# variables.
t.test(task_intensity~CONDITION, data = df, var.equal = TRUE)
t.test(task_unpleasantness~CONDITION, data = df, var.equal = TRUE)
# 3. Next, you want to examine whether the manipulation led to differences in
# positive and negative affect between the conditions. Perform t-tests to
# explore this possibility.
t.test(Pos_PANAS~CONDITION, data = df, var.equal = TRUE) 
t.test(Neg_PANAS~CONDITION, data = df, var.equal = TRUE)
# 4. You also want to examine whether the manipulation was more threatening or
# challenging for the pain condition compared to the control condition. Perform
# a t-test to explore this possibility.
t.test(Threat_MEAN~CONDITION, data = df, var.equal = TRUE) 
t.test(Challenge_MEAN~CONDITION, data = df, var.equal = TRUE)
# 5. Finally, to test the main prediction, conduct a one-way ANOVA to determine
# whether the manipulation led to a difference in group bonding. To conduct this
# test, you will first need to compute a new variable that reflects the mean of
# the seven items used to measure bonding (group101, group102, group103,
# group104, group105, group106, group107). Name your new variable BONDING_MEAN.
# Then perform a one-way ANOVA with CONDITION as the independent variable and
# BONDING_MEAN as the dependent variable.
df <- mutate(df, BONDING_MEAN = (group101 + group102 +  group103 + 
              group104 + group105 + group106 + group107)/7)

aov_car(BONDING_MEAN ~ CONDITION + Error(ID), data = df, type = "2")
 
# 6. Prepare an APA-style results section to describe each of the analyses
# conducted above.

# 7. Generate a bar graph to depict the results. Don't forget to include error
# bars that reflect the 95% confidence intervals.

out <-  df %>% 
  summarySE("BONDING_MEAN", 
            groupvars = "CONDITION", na.rm=TRUE) 

ggplot(out, aes(x= CONDITION, y=BONDING_MEAN, fill = CONDITION)) + 
  geom_col(position="dodge") + 
  geom_errorbar(aes(ymin = BONDING_MEAN - ci, ymax = BONDING_MEAN + ci, width = .2), position = position_dodge(.9)) + 
  theme_classic() 
