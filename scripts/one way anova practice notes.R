# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# WHAT DO??? HOW TO GENERAL LINEAR MODEL!!!!! :O ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# GOING TO NEED TO INSTALL SOME NEW PACKAGES FOR THE GLM ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# new packages - run this code if you don't have these packages 
# install.packages("car")
# install.packages("emmeans")
# install.packages("Hmisc")
# install.packages("multcompView")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# LOAD LIBRARIES ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# load the new libraries and existing ones 
library(tidyverse)
library(readxl)
library(skimr)
library(janitor)
library(patchwork)
library(car)
library(emmeans)
library(Hmisc)
library(multcompView)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# READ IN THE DATA ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# this reads in the data
# clean names makes the names work better in R
# remove empty removes empty rows and columns that may make things look messy,
# don't always need it, but I always run it just in case
full.df <- read_csv("data/emily_harders/Emily_Rfuntime_7Feb22.csv") %>%
  clean_names() %>%
  remove_empty(which = c("cols", "rows"))

# we need to change sampling day to a factor
full.df <- full.df %>%
  mutate(sampling_day = as.factor(sampling_day))


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# WIDE AND LONG FORMATTING  ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# its good practice to get familiar with moving data between wide and long format
# the one we loaded in is already in wide so we call it wide 
wide.df <- full.df

# pivot wide to long
long.df <- wide.df %>%
  pivot_longer(cols = c(ends_with("normalized")), 
               names_to = "name",
               values_to = "value")

# pivot long to wide bc practice is good
practice.df <- long.df %>%
  pivot_wider(names_from = "name",
              values_from = "value")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# A QUICK VISUAL  ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

long.df %>%
  ggplot(aes(x = sampling_day, y = value)) +
  geom_point(position = position_dodge2(width = 0.5), shape = 5) +
  stat_summary(fun = "mean", geom = "point", position = position_dodge2(width = 0.5)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.5, 
               position = position_dodge2(width = 0.5)) +
  facet_wrap(~name, scales = "free") +
  theme_minimal()

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# REDUCING DATA SET TO MAKE LIFE EASIER FOR A MOMENT ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# to make things simpler we are going to use filter to reduce this data sat to 
# one metric from one day
# filter works with rows and select works with columns 
# doing this in long and wide bc good practice 

r_wide.df <- wide.df %>%
  select(sampling_day, treatment, acot13_normalized) %>%
  filter(sampling_day == 4)

r_long.df <- long.df %>%
  select(sampling_day, treatment, name, value) %>%
  filter(sampling_day == 4) %>%
  filter(name == "acot13_normalized")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# ANOTHER QUICK VISUAL ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# after we reduce the dataset we want to take another look at it to see whats going on
# this is mean and standard error with points so we can see dispersion
r_wide.df %>%
  ggplot(aes(x = treatment, y = acot13_normalized, color = treatment)) +
  geom_point(position = position_dodge2(width = 0.1), shape = 5) +
  stat_summary(fun = "mean", geom = "point", position = position_dodge2(width = 0.5)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.5, 
               position = position_dodge2(width = 0.5))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# STATS ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Ho: the difference in population means between cort and oil is equal to 0

# assumptions - the ones that we will test for anyway
# variance is homogenous - shaprio wilk or visual
# residuals are normal (w/in your treatments) - levene, bartlett, or visual

# MAKE YOUR LINEAR MODEL ----

# the first thing we need to do is create our model
# you'll notice this is a little different than what we did in class because
# in class we used wide format and here I use long format
acot.lm <- lm(value ~ treatment, data = r_long.df)

# STATISTICAL TESTS FOR ASSUMPTIONS ----

# statistical test for homogeneity of variance
bartlett.test(value ~ treatment, data = r_long.df)

shapiro.test(acot.lm$residuals)

# VISUAL TESTS FOR ASSUMPTIONS ----

# visual test for normality
qqPlot(acot.lm$residuals)

par(mfrow=c(2,2))
plot(acot.lm)


# A DIFFERENT WAY TO VISUALLY LOOK AT ASSUMPTIONS ----

# now we resist the urge to go straight for the results and test the assumptions
# this code calculates the residuals from the model
residuals <- (resid(egg.lm))

# here we plot the residuals to test for homogneiety of variance - looks fine
plot(fitted(egg.lm), residuals)

# this makes a qq plot to test for normality
qqnorm(residuals)
qqline(residuals)

# RUNNING THE ANOVA ----

# our data look fine so now we run the anova, design is balanced for what we have so type 3
Anova(acot.lm, type = "3")

# treatment is not significant, fail to reject Ho
# Response: value
#               Sum Sq Df  F value    Pr(>F)    
# (Intercept) 0.248795  1 29.5287    0.0002873 ***
# treatment   0.006720  1  0.7976    0.3927905    
# Residuals   0.084255 10  

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# WHAT IF WE WANT TO DO POST-HOC ANALYSES ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# we don't have a significant difference here, but if we want to do post hoc tests here is how
# use the package emmeans, which SAS calls lsmeans 

# create a model for the emmeans, then put in what we want in the model 
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

# we create a data frame from the interaction file we made 
emm.df <- as.data.frame(egg.emminteraction)

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
# is there a difference between summary(aov) or Anova(blah blah blah)
# ANCOVA
