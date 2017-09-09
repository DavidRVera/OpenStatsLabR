# Load packages
library(tidyverse)
library(afex)
library(Rmisc)

# 1. Open the data file (called Grossmann and Kross 2014 Study 2). Explore the
# data file. Note, you will not analyze all of these variables. Try to find the
# variables that are relevant to the study description above. read data
df <-  read_csv("Grossman and Kross 2014 Study 2.CSV")

df <- mutate(df, 
             CONDITION = factor(case_when(CONDITION == 1 ~ "SI",
                                    CONDITION == 2 ~       "SD",
                                    CONDITION == 3 ~       "OI",
                                    CONDITION == 4 ~       "OD"), 
                                levels = c("SI",
                                           "SD",
                                           "OI",
                                           "OD")))

# 2. Conduct a one-way ANOVA to determine if there is a significant difference
# between the conditions on wisdom.

AoV <- aov_car(WISDOM ~ as.factor(CONDITION) + Error(ID), type = "2", data = df)
AoV
# 3. Next, you want to determine whether the self-immersed condition was
# significantly lower in wisdom, relative to the other-immersed and
# other-distanced condition. Conduct a planned contrast to test the typical
# Solomon's paradox effect.
rl <-  lsmeans(AoV, ~as.factor(CONDITION)) # Create reference grid

c1 <- list(SIvsOther =  c(-1, 0, .5, .5))

#specify contrast
contrast(rl, c1) #test contrasts on reference grid

# 4. Now, you want to show that taking a distant perspective increases wisdom
# relative to taking an immersed perspective when dealing with one's own
# problems. Conduct a planned contrast to determine whether self-distancing
# results in significantly higher levels of wisdom, relative to self-immersion.
c2 <- list(SIvsSD = c(-1, 1, 0, 0))
contrast(rl, c2)

# 5. You also want to determine whether distancing vs. immersion increases
# wisdom when contemplating other people's problems. Conduct a planned contrast
# to compare the other-distance vs. other-immersed conditions.
c3 <- list(OIvsOD =c(0, 0, -1, 1))
contrast(rl, c3) 
# 6. Finally, you want to test whether self-distancing eliminates the increased
# wisdom typically found in reasoning about others. Conduct a planned comparison
# to determine whether the self-distanced condition is significantly different
# from the other-immersed and other-distanced conditions.
c4 <-  list(SDvsO = c(0, 1, -.5, -.5))
contrast(rl, c4)

# 7. Prepare an APA-style results section to describe each of the analyses
# conducted above.

# 8. Generate a bar graph to depict the results for the one-way ANOVA. Don't
# forget to include error bars that reflect the +/- 1 standard error of the
# mean.

out <-  df %>% 
  summarySE("WISDOM", 
            groupvars = "CONDITION", na.rm=TRUE) 

ggplot(out, aes(x= CONDITION, y=WISDOM, fill = CONDITION)) + 
  geom_col(position="dodge") + 
  geom_errorbar(aes(ymin = WISDOM-se, ymax = WISDOM+se, width = .2), position = position_dodge(.9)) + 
  theme_bw() 
