
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
# remove empty is something i often add, the code removes empty rows and columns from a df
# in this case we don't need it, but it can be super handy
egg.df <- read_excel("data/emily_harders/Emily_Rfuntime_7Feb22.xlsx") %>%
  clean_names() %>%
  remove_empty(which = c("cols", "rows"))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# PLOTTING ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# call the data, run ggplot and define axes, then provide the geom
# in geom we give it position dodge to add some jitter to the points so they dont overlap 
egg.df %>%
  ggplot(aes(x = sampling_day, y = akr1d1_normalized, shape = treatment, color = treatment)) +
  geom_point(position = position_dodge2(width = 1))

# now we want to add lines between mean and se, we do this with stat summary 
# within stat summary, we have position dodge the same to make sure the mean+se lines line up
# now we add akr.plot out front so we can create a name for the plot and save it
akr.plot <- egg.df %>%
  ggplot(aes(x = sampling_day, y = akr1d1_normalized, shape = treatment, color = treatment)) +
  geom_point(position = position_dodge2(width = 1)) +
  stat_summary(fun = "mean", geom = "line", position = position_dodge2(width = 0.5)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.5, 
               position = position_dodge2(width = 0.5)) +
  labs(x = "day", y = "akr1d1")

# after naming a plot you can call it up by runing the name of the plot
akr.plot

# we do everything again with a different dependent variable 
cbr1.plot <- egg.df %>%
  ggplot(aes(x = sampling_day, y = cbr1_normalized, shape = treatment, color = treatment)) +
  geom_point(position = position_dodge2(width = 1)) +
  stat_summary(fun = "mean", geom = "line", position = position_dodge2(width = 0.5)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.5, 
               position = position_dodge2(width = 0.5)) +
  labs(x = "day", y = "cbr1")

# this is using the library patchwork  to put the plots together

# basic patchwork
akr.plot + cbr1.plot
#combine axes
akr.plot + cbr1.plot + plot_layout(guides = "collect")
# define number of columns
akr.plot + cbr1.plot + plot_layout(ncol = 1, guides = "collect")
# remove axis text that we dont want
akr.plot + theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
  cbr1.plot + plot_layout(ncol = 1, guides =  "collect")
# add annotations
akr.plot + theme(axis.title.x = element_blank(), axis.text.x = element_blank()) + 
  cbr1.plot + plot_layout(ncol = 1, guides = "collect") +
  plot_annotation(tag_levels = "A", tag_suffix = ")")
# save the plot
ggsave(akr.plot, filename = "akr.pdf", width = 5, height = 5, units = "in")

# that is one way to make multiple plots and put them together,
# patchwork = create the plot and save it, then put them together in patchwork
# another way is to use facet wrap

# our data is currently in wide format, we need it to be long to use facet wrap
long.df <- egg.df %>%
  pivot_longer(cols = c(ends_with("normalized")),
               names_to = "name",
               values_to = "value")

# normal ggplot like you always do, add geometry, and then within facet wrap you are telling
# R what to sort by, in this case we want a plot for each normalized value so we put
# name in the parantheses, which is the column for the names of the normalized values
long.df %>%
  ggplot(aes(x = sampling_day, y = value)) +
  geom_point() +
  facet_wrap(~name)

# now we make it looks nice
# scales = "free" means the y axis is autoset for each plot
long.df %>%
  ggplot(aes(x = sampling_day, y = value)) +
  geom_point(position = position_dodge2(width = 1)) +
  stat_summary(fun = "mean", geom = "line", position = position_dodge2(width = 0.5)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.5, 
               position = position_dodge2(width = 0.5)) +
  theme_light() +
  facet_wrap(~name, scales = "free")

