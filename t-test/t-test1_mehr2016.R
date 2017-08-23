# Load packages
library(tidyverse)

# read data
df <-  read_csv("Mehr Song and Spelke 2016 Experiment 1.csv")

# 1. Open the data file (called Mehr Song and Spelke Experiment 1). Explore the
# data file. Note, you will not analyze all of these variables. Try to find the
# variables that are relevant to the study description above.
names(df)
View(df)

# 2. This data file includes the variables for all 5 experiments reported in the
# paper. We only want to analyze the data for Experiment 1. Using the filter()
# function from the dplyr package, use the filter variable exp1 to select these
# cases (labeled Experiment 1 ONLY) into a new data frame. Check this data frame
# to ensure that only cases 1-32 are selected for analysis.
exp1 <- filter(df, exp1==1)

# 3. You first want to show that infants' looking behavior did not differ from 
# chance during the baseline trial. In other words, the infants did not show an 
# attentional bias prior to hearing the unfamiliar others sign the song. Perform
# a one-sample t-test to examine whether the proportion of time spent looking at
# the person singing the familiar song at baseline did not differ from chance 
# (0.5). You'll need to used the t.test function, specify the column using $
# notation, and specify the null proportion (mu).

t.test(exp1$Baseline_Proportion_Gaze_to_Singer, mu=.5)

# 4. Next, you want to demonstrate that infants attended equally to the two
# singers during the familiarization trials. Run a paired samples t-test
# comparing the Gaze to Familiar Song vs. the Gaze to Unfamiliar Song. This
# time, instaid of a mu, you'll need to specify two columns and that the test is
# paired.
t.test(exp1$Familiarization_Gaze_to_Familiar, exp1$Familiarization_Gaze_to_Unfamiliar, paired = TRUE)


# 5. Now, perform a one-sample t-test to examine whether the proportion of infants' looking behavior
# toward the singer of the familiar melody was higher than chance at the test phase (0.5).
t.test(exp1$Test_Proportion_Gaze_to_Singer, mu=.5)

# 6. Finally, compare looking behavior at baseline to looking behavior at test, using a paired-samples
# t-test.

t.test(exp1$Baseline_Proportion_Gaze_to_Singer, exp1$Test_Proportion_Gaze_to_Singer, paired = TRUE)

# 7. Prepare an APA-style results section to describe each of the analyses conducted above.

# 8. Generate a boxplot to depict the proportion of time infants spent looking at the singer of the
# familiar song at the baseline and test trials.

# Because we need to represent the same cases (individuals) twice in the same 
# graph, we need to reorganise the data first, so it has all of the gaze 
# proportions as one variable, but with a seperate variable indicating whether 
# we mean baseline or test. This can be achieved pretty easily with the gather 
# function from the  tidyr package. From then it should be relatively simple to
# make a ggplot using the geom_boxplot.
plot <- gather(exp1, key = measure, 
               value = proportion_gaze, Baseline_Proportion_Gaze_to_Singer, 
               Test_Proportion_Gaze_to_Singer)

ggplot(plot) + geom_boxplot(aes(y= proportion_gaze, x=measure))

# 9. Generate a scatterplot (using the ggplot function and geom_point) to depict
# the relationship between the estimated number of times the infants heard the
# song and their increased looking behavior from the baseline to test trials.
ggplot(exp1, aes(x = Estimated_Total_Number_of_Song, y = Difference_in_Proportion_Looking, xmin = 5, xmax = 320)) + 
  geom_point() + geom_smooth(method = "lm")


