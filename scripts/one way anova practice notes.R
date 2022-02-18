# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# WHAT DO??? HOW TO GENERAL LINEAR MODEL!!!!! :O ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# GOING TO NEED TO INSTALL SOME NEW PACKAGES FOR THE GLM ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# new packages
install.packages("car")
install.packages("emmeans")
install.packages("Hmisc")
install.packages("multcompView")

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
# we will use filter for rows and select for columns from both datasets

r_wide.df <- wide.df %>%
  select(sampling_day, treatment, akr1d1_normalized) %>%
  filter(sampling_day == 4)

r_long.df <- long.df %>%
  select(sampling_day, treatment, name, value) %>%
  filter(sampling_day == 4) %>%
  filter(name == "akr1d1_normalized")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# ANOTHER QUICK VISUAL ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

r_wide.df %>%
  ggplot(aes(x = treatment, y = akr1d1_normalized, color = treatment)) +
  geom_point(position = position_dodge2(width = 0.1), shape = 5) +
  stat_summary(fun = "mean", geom = "point", position = position_dodge2(width = 0.5)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.5, 
               position = position_dodge2(width = 0.5))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# STATS ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Ho: the difference in population means between cort and oil treatments is equal to 0

# assumptions - the ones that we will test for anyway
# variance is homogenous
# residuals are normal

# the first thing we need to do is create our model
egg.lm <- lm(value ~ treatment, data = r_long.df)

# now we resist the urge to go straight for the results and test the assumptions
# this code calculates the residuals from the model
residuals <- (resid(egg.lm))

# here we plot the residuals to test for homogneiety of variance - looks fine
plot(fitted(egg.lm), residuals)

# this makes a qq plot to test for normality
qqnorm(residuals)
qqline(residuals)

# our data look fine so now we run the anova, design is balanced for what we have so type 3
Anova(egg.lm, type = "3")

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

# create a model for the emmeans
egg.emm <- emmeans(egg.lm, ~ treatment)
# plot the emmeans - if they arrows overlap fail to reject Ho
plot(egg.emm, comparisons = TRUE)
# this creates a model to do interactions and impliment p-value adjustment
egg.emminteraction <- emmeans(egg.emm, pairwise ~ treatment, adjust = "bonferroni")
egg.emminteraction$emmeans
egg.emminteraction$contrasts
