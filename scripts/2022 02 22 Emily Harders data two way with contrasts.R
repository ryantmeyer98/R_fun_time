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
  # filter(sampling_day == 4) %>% 
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

# how can we work with just this acot data
acot.df <- egg.df %>% 
  select(egg_id, day = sampling_day, treatment, acot13_normalized) 
  
# now the sampling day is a number and needs to be a factor ---
acot.df <- acot.df %>% 
  mutate(day = as.factor(day),
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
# Set it up for Type III SS ANOVA
options(contrasts = c("contr.sum", "contr.poly"))


# the first thing we need to do is create our model
acot.lm <- lm(acot13_normalized ~ treatment*day, data = acot.df)

# note you can add a log() for the acot and it will do the log of the value
# and all looks better

# Test Assumptions ----
# Assumption 1 - Homogeneity of variance ---- 
## Bartlets test -----
# significant effect means they differ in variances and not good
bartlett.test(acot13_normalized ~ treatment*day, data=acot.df)

##  Levene's test ----
leveneTest(acot13_normalized ~ treatment*day, data=acot.df)

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


# not can use %>% multcomp::cld(Letters=letters) 


# so this is fun as there is no interaction but the main effects differ
# so lets look first at day
day.emm <- emmeans(acot.lm, ~ day)
# plot it 
plot(day.emm, comparisons = TRUE)
# this creates a model to do interactions and impliment p-value adjustment
day.emminteraction <- emmeans(day.emm, pairwise ~ day, adjust = "bonferroni")
day.emminteraction$emmeans
day.emminteraction$contrasts

# now we could test for treatment
treat.emm <- emmeans(acot.lm, ~ treatment)
# plot it 
plot(treat.emm, comparisons = TRUE)
# this creates a model to do interactions and impliment p-value adjustment
day.emminteraction <- emmeans(treat.emm, pairwise ~ treatment, adjust = "bonferroni")
day.emminteraction$emmeans
day.emminteraction$contrasts



# what if you wanted to do pairwise comparisons only within day
# planned contrasts within time
emm.int <- emmeans(acot.lm,  ~ treatment*day)
emm.int

contrast(emm.int, method = "pairwise", by = "day") 



# manual method
emm.int_long <- emmeans(acot.lm, list(~ treatment*day))
emm.int_long

levels(acod.df$day*acot.df$treatment)

contrast <- list(
  oil_cort_4  = c(1,-1,0,0,0,0,0,0),
  oil_cort_6  = c(0,0,1,-1,0,0,0,0),
  oil_cort_8  = c(0,0,0,0,-1,1,0,0),
  oil_cort_10 = c(0,0,0,0,0,0,-1,1))

contrast(emm.int_long, contrast)

emm.int_long <- emmeans(emm.int_long, method = contrast, adjust = "mvt")
emm.int_long
