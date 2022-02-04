# Install new packages -----
# Packages already installed
# install.packages("tidyverse")
# install.packages("readxl")

# Packages to install and run library one time
install.packages("ggThemeAssist")
install.packages("styler")

# install packages in case you did not get tthem
install.packages("skimr")
install.packages("janitor")
install.packages("patchwork")

# Load these libraries only once ----
# library(ggThemeAssist)
# library(styler)

# load Libraries that we use in all scripts -----
library(tidyverse)
library(readxl)
library(skimr)
library(janitor)
library(patchwork)

# Load the data 
egg.df   <- read_csv("data/emily_harders/Experiment3_corttimecourse_datasheet_1Feb22 - Sheet9.csv") %>% clean_names()


egg.df %>% 
  ggplot(aes(sampling_day, akr1d1_normalized, color=treatment)) +
  stat_summary(fun = mean, na.rm = TRUE,
               geom = "point",
               size = 3, position = position_dodge(width=0.4)) + 
  stat_summary(fun.data = mean_se, na.rm = TRUE,
               geom = "errorbar",
               width = 0.2, position = position_dodge(width=0.4))

egg.df %>% 
  ggplot(aes(sampling_day, cbr1_normalized, color=treatment)) +
  stat_summary(fun = mean, na.rm = TRUE,
               geom = "point",
               size = 3, position = position_dodge(width=0.4)) + 
  stat_summary(fun.data = mean_se, na.rm = TRUE,
               geom = "errorbar",
               width = 0.2, position = position_dodge(width=0.4))


egg.df %>% 
  ggplot(aes(sampling_day, fkbp51_normalized, color=treatment)) +
  stat_summary(fun = mean, na.rm = TRUE,
               geom = "point",
               size = 3, position = position_dodge(width=0.4)) + 
  stat_summary(fun.data = mean_se, na.rm = TRUE,
               geom = "errorbar",
               width = 0.2, position = position_dodge(width=0.4))


egg.df %>% 
  # filter(sampling_day==4) %>% 
  ggplot(aes(sampling_day, acot13_normalized, color=treatment)) +
  stat_summary(fun = mean, na.rm = TRUE,
               geom = "point",
               size = 3, position = position_dodge(width=0.4)) + 
  stat_summary(fun.data = mean_se, na.rm = TRUE,
               geom = "errorbar",
               width = 0.2, position = position_dodge(width=0.4))


egg.df %>% 
  group_by(sampling_day) %>% skim()
