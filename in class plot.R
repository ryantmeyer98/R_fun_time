

# load libraries =---
library(tidyverse)
library(readxl)
library(janitor)
library(patchwork)

egg.df <- read_excel("data/emily_harders/Emily_Rfuntime_7Feb22.xlsx") %>% 
  clean_names()


ak.plot<- egg.df %>% 
  ggplot(aes(sampling_day, akr1d1_normalized, shape=treatment, color=treatment)) +
  geom_point(position = position_dodge2(width = 1)) +
  stat_summary(
    fun=mean, na.rm=TRUE,
    geom="line",
    position = position_dodge2(width = .5)
  )+
  stat_summary(
    fun.data =mean_se, na.rm=TRUE,
    geom="errorbar",
    width=.5,
    position = position_dodge2(width = .5)
  ) +
  labs(x="day", y="akr1d1")
ak.plot

cb.plot<- egg.df %>% 
  ggplot(aes(sampling_day, cbr1_normalized, shape=treatment, color=treatment)) +
  geom_point(position = position_dodge2(width = 1)) +
  stat_summary(
    fun=mean, na.rm=TRUE,
    geom="line",
    position = position_dodge2(width = .5)
  )+
  stat_summary(
    fun.data =mean_se, na.rm=TRUE,
    geom="errorbar",
    width=.5,
    position = position_dodge2(width = .5)
  ) +
  labs(x="day", y="CBRT")
cb.plot


ak.plot + theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
  cb.plot + theme()+
  plot_layout(ncol= 1, guides = "collect") +
  plot_annotation(tag_levels = "A", tag_suffix = ")")

ggsave(cb.plot, filename = "cb.pdf", width  = 5, height = 5, units="in")

