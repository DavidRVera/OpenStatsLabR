# Load packages
library(tidyverse)
library(afex)
library(Rmisc)

# 1. Open the data file (called Maglio and Polman Experiment 1). Explore the data file.
# read data
df <-  read_csv("Maglio and Polman 2014 Experiment 1.csv")
df <-  df %>% mutate(ID= 1:NROW(df), 
                     station = factor(case_when( 
                                      station == 1 ~ "Spadina", 
                                      station == 2 ~ "St. George", 
                                      station == 3 ~ "Bloor-Yonge", 
                                      station == 4 ~ "Sherbourne"),
                                      levels = c("Spadina", 
                                      "St. George", 
                                      "Bloor-Yonge",
                                      "Sherbourne")),
                     orientation = factor(case_when(
                       orientation == 1 ~ "Travelling East",
                       orientation == 2 ~ "Travelling West"
                     )))
 
# 2. Perform the two-way ANOVA to test whether orientation (toward or away from)
# interacts with Station (Spadina, St. George, Bloor-Yonge, Sherbourne).
aov_car(subjective_distance ~ orientation * station + Error(ID), 
        data = df, type= "3")
 
# 3. Split the file based on Station.
df1 <- filter(df, station== "Spadina")
df2 <- filter(df, station== "St. George") 
df3 <- filter(df, station== "Bloor-Yonge")
df4 <- filter(df, station== "Sherbourne")

# 4. Perform independent samples t-tests to determine if orientation is
# significantly different at each station.
t.test(subjective_distance~orientation, var.equal = TRUE, data=df1)
t.test(subjective_distance~orientation, var.equal = TRUE, data=df2)
t.test(subjective_distance~orientation, var.equal = TRUE, data=df3)
t.test(subjective_distance~orientation, var.equal = TRUE, data=df4)
 
# 5. Generate a line graph to depict the interaction between orientation and station.

ggplot(df, aes(x = station, y = subjective_distance, color = orientation, group = as.factor(orientation))) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") + 
  theme_classic()
 
# 6. Prepare an APA-style results section to describe each of the analyses
# conducted above.
