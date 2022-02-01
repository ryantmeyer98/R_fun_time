# Install Packages -----
## This section is for installing packages, you only need to do this once
install.packages("tidyverse") # installs several libraries that are useful
install.packages("readxl") # istalls ability to read excel files

# Load Libraries ----
## This section loads the libraries (packages). Each time you run a script you should
## run load library and I copy this from script to script.
library(tidyverse)
library(readxl)

# Homework ----
## You homework is to practice installing packages and loading libraries 
## Packages of interest are 
## - janitor 
## - skimr

# Loading data ----
## to load data the syntax reads
## data.df <- read_csv("/data/name_of_file.csv")
## stored object taken from command to read and path and name of file
## read_csv is a tidyverse command
## read.csv is a base_r command 
## both do the same thing essentially

# Read in a csv file -----
mm.df <- read_csv("data/mms/mms.csv")

# read in an excel file ----
mm.df <- read_????("")

# How to look at data ----
# 1) click on the name of the dataframe in upper right
# 2) type and run - name_of_dataframe
# 3) type and run - head(name_of_dataframe)
# 4) type and run - tail(name_of_dataframe)
# 5) to see just one column type and run name_of_dataframe$variable

# How to modify data ----
## Lets calculate the area of the m and m from diameter
## sa = pi * r^2

# Base R approach
mm.df$sa <- ((mm.df$diameter)/2)^2 * pi

# Long Tidyverse approach
mm.df <- mm.df %>% 
  mutate(radius = diameter/s)

mm.df <- mm.df %>% 
  mutate(sa = ((radius)/2)^2 * pi)

# what are two ways you could shorten this?
 

   
# method with less typing 
# yeah you could do this too but that involves math
mm.df <- mm.df %>% 
  mutate(sa = ((diameter)/2)^2 * pi)



# Making a graph ---
# GGPlot

# ggplot(dataframe, aes(x = x_variable, y = y_variable)) +
#   geom_type()

ggplot(mm.df, aes(x = diameter, y = mass)) 
