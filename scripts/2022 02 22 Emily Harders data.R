# This week we will do an ANOVA and then get more complicated if you want
# we will start with a one way on day 4 data
# 
# # install packages for doing anova ----
# install.packages("car")
# install.packages("emmeans")
# install.packages("Hmisc")
# install.packages("multcompView")


# Load Libraries ----
library(tidyverse)
library(readxl)
library(patchwork)
library(janitor)

# Libraries for ANOVA ----
library(car)
library(emmeans)


# read in data ----
egg.df <- read_excel("data/emily_harders/Emily_Rfuntime_7Feb22.xlsx") %>% 
  clean_names()

# lets look at the data for akr1d1
egg.df %>% 
  ggplot(aes(sampling_day, akr1d1_normalized)) +
  geom_point()

# plot of AKR1 ----
acot.plot <- egg.df %>% 
  ggplot(aes(sampling_day, acot13_normalized, color = treatment)) +
  geom_point(position = position_dodge2(width=.5))+
  stat_summary(
    fun = mean, na.rm=TRUE,
    geom = "point",
    size = 5,
    position = position_dodge2(width=.5)) +
  stat_summary(
    fun.data = mean_se, na.rm=TRUE,
    geom = "errorbar",
    width = 0.5,
    position = position_dodge2(width=.5))
acot.plot

# what if we wanted to only look at day 4 ----
acot.plot <- egg.df %>% 
  filter(sampling_day == 4) %>% 
  ggplot(aes(sampling_day, acot13_normalized, color = treatment)) +
  geom_point(position = position_dodge2(width=.5))+
  stat_summary(
    fun = mean, na.rm=TRUE,
    geom = "point",
    size = 5,
    position = position_dodge2(width=.5)) +
  stat_summary(
    fun.data = mean_se, na.rm=TRUE,
    geom = "errorbar",
    width = 0.5,
    position = position_dodge2(width=.5))
acot.plot

# how can we work with just this data
acot.df <- egg.df %>% 
  select(egg_id, sampling_day, treatment, acot13_normalized) %>% 
  filter(sampling_day == 4)
acot.df

# now the sampling day is a number and needs to be a factor ---
acot.df <- acot.df %>% 
  mutate(sampling_day = as.factor(sampling_day),
         treatment = as.factor(treatment))

levels(acot.df$treatment)

# reorder the levels
acot.df <-  acot.df %>% 
  mutate(treatment = fct_relevel(treatment, "oil", "cort"))

# ANOVA TEST HYPOTHESES----
# Null - the means of the various groups do not differ
# Alternate - at least one of the means differs signifcantly from others

# Assumnptions
# Observations are obtained independently and randomly from the population 
# Data of each factor level are normally distributed
# Normal populations have a common variance - Levene's or Bartlett test 

# Set up the Anova Model ----
# the first thing we need to do is create our model
acot.lm <- lm(acot13_normalized ~ treatment, data = acot.df)

# Test Assumptions ----
# Assumption 1 - Homogeneity of variance ---- 
## Bartlets test -----
# significant effect means they differ in variances and not good
bartlett.test(acot13_normalized ~ treatment, data=acot.df)

##  Levene's test ----
leveneTest(acot13_normalized ~ treatment, data=acot.df)

# Assumption 2 - Nrmally distributed data within factor levels -----
# QQ-plot
qqPlot(acot.lm$residuals) 

# Shapiro Wilks test ---
shapiro.test(acot.lm$residuals)

# Assumption test short cut---
par(mfrow=c(2,2))
plot(acot.lm)

# Now we run the anova
# Sum of Squares type is important especially for above one way
# if unbalanced - unequal samples per treatment then use type = "3" 
Anova(acot.lm, type = "3")


# now what to do - Post hoc comparison - for one  way with 2 treatments not needed but
# create a model for the emmeans
acot.emm <- emmeans(acot.lm, ~ treatment)
# plot the emmeans - if they arrows overlap fail to reject Ho
plot(acot.emm, comparisons = TRUE)
# this creates a model to do interactions and impliment p-value adjustment
acot.emminteraction <- emmeans(acot.emm, pairwise ~ treatment, adjust = "bonferroni")
acot.emminteraction$emmeans
acot.emminteraction$contrasts

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# NOW WHAT IF WE WANT TO PLOT EMMEANS? ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# NOT GONNA LIE, THERE IS KIND OF A LOT GOING ON HERE

# from my understanding, which may be flawed, 
# this line of code creates a file of integers from the emmeans
acot.int = as.data.frame(emmeans(acot.emm, pairwise ~ treatment, adjust = "bonferroni"))

# call the data frame, filter out the "." it seems to add by default,
# plot the x axis for treatment and add color as treatment, 
# we need to define the point as the emmean, 
# we then add the error bars manually
# we do it this way because the error and means have already been caluclated,
# we just need to define them 
emm.df %>%
  filter(treatment != ".") %>%
  ggplot(aes(x = treatment, color = treatment)) +
  geom_point(aes(y = emmean)) +
  geom_errorbar(aes(ymin = emmean-SE, ymax = emmean+SE), stat = "identity") +
  expand_limits(y = 0)

# the end :) 

