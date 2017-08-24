# Load packages
library(tidyverse)

# read data
df <-  read_csv("Beall Hofer Schaller Study 1.csv")

# 1. Open the data file (called Beall Hofer and Schaller 2016 Study 1). Explore
# the data file. Note, you will not analyze all of these variables. Try to find
# the variables that are relevant to the study description above.
View(df)

# 2. Run a correlation analysis to test if there is an association between the
# Ebola search volume index and the voter intention index. Use the cor.test function
cor.test(df$Ebola.Search.Volume.Index, df$Voter.Intention.Index)

# 3. Next, to test whether the association between these variables is stronger
# during the period just prior to and after the Ebola outbreak, select only the
# scores from the two-week period including the last week of September and the
# first week of October. Re-run the correlation analyses for the association
# between Ebola search volume index and voter intention index.
df1 <- filter(df, (Month == 9 & Date > 23) | (Month == 10 & Date < 8))
cor.test(df1$Daily.Ebola.Search.Volume, df1$Voter.Intention.Index)

# 4. Prepare a series of scatterplots (making sure to follow APA-style guidelines). 
# First, depict the relationship between day and the voter intention index for
# the month of September. 
df %>% filter(Month == 9) %>% 
  ggplot(aes(x = Date, y = Voter.Intention.Index)) + geom_point() + theme_classic()

# Second, depict the relationship between day and the voter intention index for
# the last week of September. 
df %>% filter(Month == 9  & Date > 23) %>% 
  ggplot(aes(x = Date, y = Voter.Intention.Index)) + geom_point() + theme_classic()
 
# Third, depict the relationship between day and the voter intention index for
# the month of October.
df %>% filter(Month == 10) %>% 
  ggplot(aes(x = Date, y = Voter.Intention.Index)) + geom_point() + theme_classic()

# Finally, depict the relationship between day and the voter intention index for
# the first week of October.
df %>% filter(Month == 10 & Date < 8) %>% 
  ggplot(aes(x = Date, y = Voter.Intention.Index)) + geom_point() + theme_classic()
