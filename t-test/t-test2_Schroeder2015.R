# Load packages
library(tidyverse)
library(Rmisc)

# read data
df <-  read_csv("Schroeder and Epley 2015 Study 4 data.csv")

# 1. Open the data file (called Schroeder and Epley 2015 Experiment 4 data).
# Explore the data file. Note, you will not analyze all of these variables. Try
# to find the variables that are relevant to the study description above.
View(df)

# 2. You first want compare participants in the audio condition to participants
# in the transcript condition on the Intellect_Rating variable. Which type of
# analysis is appropriate, given the design described above?
t.test(Intellect_Rating ~ CONDITION, data=df, var.equal=TRUE) 

# 3. Next compare participants in the audio condition to participants in the transcript condition on the
# Impression_Rating variable.
t.test(Impression_Rating ~ CONDITION, data=df, var.equal=TRUE) 
# 4. Finally, compare participants in the audio condition to participants in the
# transcript condition on the Hire_Rating variable
t.test(Hire_Rating ~ CONDITION, data=df, var.equal=TRUE) 

# 5. Prepare an APA-style results paragraph describing the results of the
# analyses performed above.

# 6. Generate a figure to depict the results of the analyses performed above. 
# Make sure to follow APA guidelines, and include error bars representing +/- 1 
# standard error of the mean. The Rmisc package will help you get the summary
# stats you need.

out <-  df %>% gather(key = Domain, 
                       value = rating, 
                       Intellect_Rating, 
                       Impression_Rating, 
                       Hire_Rating) %>% 
                summarySE("rating", 
                          groupvars = c("CONDITION", "Domain")) 

ggplot(out, aes(x= Domain, y=rating, fill = as.factor(CONDITION))) + 
  geom_col(position="dodge") + 
  geom_errorbar(aes(ymin = rating-se, ymax = rating+se, width = .2), position = position_dodge(.9)) + 
  theme_bw() 

