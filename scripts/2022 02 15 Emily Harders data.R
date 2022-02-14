
# this is an example of graphing data from basic to complex
# we will use patchwork for some of the things

# Load Libraries ----
library(tidyverse)
library(readxl)
library(patchwork)
library(janitor)


# read in data ----
egg.df <- read_excel("data/emily_harders/Emily_Rfuntime_7Feb22.xlsx") %>% 
  clean_names()

# lets look at the data for akr1d1
egg.df %>% 
  ggplot(aes(sampling_day, akr1d1_normalized)) +
  geom_point()



# maybe we want to see the data not overlapping
egg.df %>% 
  ggplot(aes(sampling_day, akr1d1_normalized)) +
  geom_point(position = position_dodge2(width=.5))

# what if we wanted to see the mean
egg.df %>% 
  ggplot(aes(sampling_day, akr1d1_normalized)) +
  geom_point(position = position_dodge2(width=.5))+
  stat_summary(
    fun = mean, na.rm=TRUE,
    geom = "point",
    size = 5,
    color= "red")

# now if we wanted the standard error
egg.df %>% 
  ggplot(aes(sampling_day, akr1d1_normalized)) +
  geom_point(position = position_dodge2(width=.5))+
  stat_summary(
    fun = mean, na.rm=TRUE,
    geom = "point",
    size = 5,
    color= "red") +
  stat_summary(
    fun.data = mean_se, na.rm=TRUE,
    geom = "errorbar",
    width = 0.2,
    color="red")

# but wait we have a treatement
# now if we wanted the standard error
egg.df %>% 
  ggplot(aes(sampling_day, akr1d1_normalized, color = treatment)) +
  geom_point(position = position_dodge2(width=.5))+
  stat_summary(
    fun = mean, na.rm=TRUE,
    geom = "point",
    size = 5,
    color= "red") +
  stat_summary(
    fun.data = mean_se, na.rm=TRUE,
    geom = "errorbar",
    width = 0.2,
    color="red")

# now wait there shoudl be 2 means and se bars
ak.plot <- egg.df %>% 
  ggplot(aes(sampling_day, akr1d1_normalized, color = treatment)) +
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
ak.plot


# now lets do cbr1
ac.plot <- egg.df %>% 
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
ac.plot

# add in lines
# now lets do cbr1
ac.plot <- egg.df %>% 
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
ac.plot

# now lets put on the same page
ak.plot + ac.plot

# now gather legends
ak.plot + 
  ac.plot + theme(axis.title.y = element_blank())+
  plot_layout(guides = "collect")

# Maybe we want it as a top and bottom
ak.plot +  theme(axis.title.x = element_blank(),
                 axis.text.x = element_blank()) + 
  ac.plot +
  plot_layout(
    ncol=1,
    guides = "collect") 

# Maybe we want to add labels
ak.plot +  theme(axis.title.x = element_blank(),
                 axis.text.x = element_blank()) + 
  ac.plot +
  plot_layout(
    ncol=1,
    guides = "collect")  +
  plot_annotation( tag_levels = "A",
                  tag_suffix = ")") 

# ok you want lines
# Maybe we want it as a top and bottom
ak.plot +  theme(axis.title.x = element_blank(),
                 axis.text.x = element_blank()) + 
  ac.plot +
  plot_layout(
    ncol=1,
    guides = "collect") 


# here is an easier way...
# pivot longer

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


# what if you wanted just one?
egg_long.df %>% 
  filter(gene == "akr1d1_normalized") %>% 
  ggplot(aes(sampling_day, value, color=treatment)) +
  geom_point(position = position_dodge2(width=.5))+
  stat_summary(fun = mean, na.rm = TRUE,
               geom = "point",
               size = 3, position = position_dodge(width=0.4)) + 
  stat_summary(fun = mean, na.rm = TRUE,
               geom = "line",
               size = 1, position = position_dodge(width=0.4),
               linetype = "dotted") + 
  stat_summary(fun.data = mean_se, na.rm = TRUE,
               geom = "errorbar",
               width = 0.2, position = position_dodge(width=0.4)) +
  facet_wrap(~gene, scales = "free_y") +
  labs(x = "Day", y = "Normalized AKR1D1 Expression (Mean ± 1 SE)") +
  scale_color_manual(
    name = "Treatment",
    labels = c("Corticosterone", "Oil"),
    values = c("blue", "red")
  )


# reorder the levels
egg_long.df <-  egg_long.df %>% 
  mutate(treatment = fct_relevel(treatment, "oil", "cort"))

# what if you wanted just one?
egg_long.df %>% 
  filter(gene == "akr1d1_normalized") %>% 
  ggplot(aes(sampling_day, value, color=treatment)) +
  geom_point(position = position_dodge2(width=.5))+
  stat_summary(fun = mean, na.rm = TRUE,
               geom = "point",
               size = 3, position = position_dodge(width=0.4)) + 
  stat_summary(fun = mean, na.rm = TRUE,
               geom = "line",
               size = 1, position = position_dodge(width=0.4),
               linetype = "dotted") + 
  stat_summary(fun.data = mean_se, na.rm = TRUE,
               geom = "errorbar",
               width = 0.2, position = position_dodge(width=0.4)) +
  facet_wrap(~gene, scales = "free_y") +
  labs(x = "Day", y = "Normalized AKR1D1 Expression (Mean ± 1 SE)") +
  scale_color_manual(
    name = "Treatment",
    labels = c("Oil", "Corticosterone"),
    values = c("red", "blue" )
  )

