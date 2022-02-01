# USEFUL RESOURCES ----
# bill's website 
# https://wlperry.github.io/2018_stats/index.html

# stack overflow
# https://stackoverflow.com 

# how to ask a stack overflow question
# https://stackoverflow.com/help/how-to-ask

# R for data science
# https://r4ds.had.co.nz

# USEFUL LIBRARIES ----
# this code install the libraries
# Libraries I use on a regular basis 
install.packages("devtools") # install new things but big
install.packages("tidyverse") # dplyr and piping and ggplot etc
install.packages("lubridate") # dates and times
install.packages("readxl") # read in excel files

# for later on we will install these 
# these are often used a lot
install.packages("scales") # scales on ggplot ases
install.packages("skimr") # quick summary stats
install.packages("janitor") # clean up excel imports
install.packages("patchwork") # arrange multiple plots per page
install.packages("plotly") # cool ggplot things

# this is in a slighly different order than above, i didn't want to reorder them
# this loads libraries, libraries are installed once, but run each time you use R 
library(tidyverse)
library(lubridate)
library(scales)
library(readxl)
library(skimr)
library(janitor)
library(patchwork)
library(plotly)
library(colorRamps)
library(car)
library(emmeans)

