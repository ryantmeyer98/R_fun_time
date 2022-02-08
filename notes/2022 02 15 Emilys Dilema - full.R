# # Install new packages -----
# # Packages already installed
# # install.packages("tidyverse")
# # install.packages("readxl")
# 
# # Packages to install and run library one time
# install.packages("ggThemeAssist")
# install.packages("styler")
# 
# # install packages in case you did not get tthem
# install.packages("skimr")
# install.packages("janitor")
# install.packages("patchwork")
# 
# # Load these libraries only once ----
# # library(ggThemeAssist)
# # library(styler)

# load Libraries that we use in all scripts -----
library(tidyverse)
library(readxl)
library(skimr)
library(janitor)
library(patchwork)

# Load the data 
egg.df   <- read_excel("data/emily_harders/Emily_Rfuntime_7Feb22.xlsx") %>% 
  clean_names()

# do you want to see quick summary stats?
# try this : skim(name of dataframe)

# then try this dataframe %>% group_by(treatment, sampling_day) %>% skim()

# Making plots
# plot 1
egg.df %>% 
  ggplot(aes(sampling_day, akr1d1_normalized, color=treatment)) +
  stat_summary(fun = mean, na.rm = TRUE,
               geom = "point",
               size = 3, position = position_dodge(width=0.4)) + 
  stat_summary(fun.data = mean_se, na.rm = TRUE,
               geom = "errorbar",
               width = 0.2, position = position_dodge(width=0.4))

# Plot 2
egg.df %>% 
  ggplot(aes(sampling_day, cbr1_normalized, color=treatment)) +
  stat_summary(fun = mean, na.rm = TRUE,
               geom = "point",
               size = 3, position = position_dodge(width=0.4)) + 
  stat_summary(fun.data = mean_se, na.rm = TRUE,
               geom = "errorbar",
               width = 0.2, position = position_dodge(width=0.4))

# plot 3
egg.df %>% 
  ggplot(aes(sampling_day, fkbp51_normalized, color=treatment)) +
  stat_summary(fun = mean, na.rm = TRUE,
               geom = "point",
               size = 3, position = position_dodge(width=0.4)) + 
  stat_summary(fun.data = mean_se, na.rm = TRUE,
               geom = "errorbar",
               width = 0.2, position = position_dodge(width=0.4))

# plot 4
egg.df %>% 
  ggplot(aes(sampling_day, acot13_normalized, color=treatment)) +
  stat_summary(fun = mean, na.rm = TRUE,
               geom = "point",
               size = 3, position = position_dodge(width=0.4)) + 
  stat_summary(fun.data = mean_se, na.rm = TRUE,
               geom = "errorbar",
               width = 0.2, position = position_dodge(width=0.4))

# so this is really time consuming
# we can speed this up with a trick
# The data is in wide format wiht columns for each response variable
# we want to make this long format where there is a column of treatment name
# and a column for the values and the rest of the information is repeated for 
# each block

egg_long.df <- egg.df %>% 
  pivot_longer(
  cols = c("akr1d1_normalized", "cbr1_normalized", 
           "fkbp51_normalized", "acot13_normalized"),
  names_to = "gene",
  values_to = "value")


egg_long.df <- egg_long.df %>% 
  arrange(gene, treatment, sampling_day)
# Now we can plot a lot easier

egg_long.df %>% 
  ggplot(aes(sampling_day, value, color=treatment)) +
  stat_summary(fun = mean, na.rm = TRUE,
               geom = "point",
               size = 3, position = position_dodge(width=0.4)) + 
  stat_summary(fun.data = mean_se, na.rm = TRUE,
               geom = "errorbar",
               width = 0.2, position = position_dodge(width=0.4)) +
  facet_wrap(~gene, scales = "free_y")

