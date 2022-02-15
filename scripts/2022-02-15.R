
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# MORE GRAPHING AND STUFF AND MAYBE SOME STATS ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# adding the 4 minus signs after a line of code adds a bookmark,
# so you can call it up easier later 

#first always load libraries
library(tidyverse)
library(readxl)
library(skimr)
library(janitor)
library(patchwork)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# READ IN THE DATA ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# read in the data 
# we create the data frame, tell R where to pull the data from,
# and then tell it to clean the names
egg.df <- read_excel("data/emily_harders/Emily_Rfuntime_7Feb22.xlsx") %>%
  clean_names()

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# PLOTTING ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# call the data, run ggplot and define axes, then provide the geom
# in geom we give it position dodge to add some jitter to the points so they dont overlap 
egg.df %>%
  ggplot(aes(x = sampling_day, y = akr1d1_normalized, shape = treatment, color = treatment)) +
  geom_point(position = position_dodge2(width = 1))

# now we want to add lines between mean and se, we do this with stat summary 
# within stat summary, we have position dodge to make sure the mean+se lines line up
# now we add akr.plot out front so we can create a name for the plot and save it
akr.plot <- egg.df %>%
  ggplot(aes(x = sampling_day, y = akr1d1_normalized, shape = treatment, color = treatment)) +
  geom_point(position = position_dodge2(width = 1)) +
  stat_summary(fun = "mean", geom = "line", position = position_dodge2(width = 0.5)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.5, 
               position = position_dodge2(width = 0.5)) +
  labs(x = "day", y = "akr1d1")

# we do everything again with a different dependent variable 
cbr1.plot <- egg.df %>%
  ggplot(aes(x = sampling_day, y = cbr1_normalized, shape = treatment, color = treatment)) +
  geom_point(position = position_dodge2(width = 1)) +
  stat_summary(fun = "mean", geom = "line", position = position_dodge2(width = 0.5)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.5, 
               position = position_dodge2(width = 0.5)) +
  labs(x = "day", y = "cbr1")

# this is using the library patchwork  to put the plots together

akr.plot + theme(axis.title.x = element_blank(), axis.text.x = element_blank()) + 
  cbr1.plot + plot_layout(ncol = 1, guides = "collect") +
  plot_annotation(tag_levels = "A", tag_suffix = ")")

ggsave(akr.plot, fulename = "akr.pdf", width = 5, height = 5, units = "in")

# use facet wrap ! 


