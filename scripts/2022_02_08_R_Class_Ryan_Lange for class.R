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
library(ggThemeAssist)
library(styler)

# load Libraries that we use in all scripts -----
library(tidyverse)
library(readxl)
library(skimr)
library(janitor)
library(patchwork)

# Load the data 
# do for left
left.df   <- read_csv("data/ryan_lange/Cottonwood_Marble_Canyon_L.csv") %>% clean_names()
# right
right.df  <- read_csv("data/ryan_lange/Cottonwood_Marble_Canyon_R.csv") %>% clean_names()
# center
center.df <- read_csv("data/ryan_lange/Cottonwood_Marble_Canyon_C.csv") %>% clean_names()

# note if you load these everything is capitalized and tough to type
# janitor can clean up the names - add    
#           %>% clean_names()
# now everything is fixed.

# making plots of elevation versus distance
left.df %>% 
  ggplot( data=   , aes(x=,   y=  )) +
  geom_xxx()

# now how can we plot all three lines on one graph
# it is possible - but it is a pain - see this mess
  ggplot() + 
    geom_line(data=left.df, aes(x=distance_m, y=elevation_m), color="black")

# Combine Files ---
# here we will bind the rows together top to bottom using bind_rows(dataframes to bind)
plain.df <-  
  
# now lets plot this
plain.df %>% 
  

# what is missing - some sort of mapping of shape or color to the name
  # copy the above and add in grouping value = ,color=XXXX


# now to add a layer of the labels of the graph
# command is labs=(x="stuff", y="stuff")
# + labs(x="",  y="")
  

# Now to make the lines the color you want with new names?
# this is a bit longer 
# each color, fill, shape, line_type have to be set if you set it.
# +
#   scale_(color, shape, line_type)_manual(
#     name = "Name of legend title",
#     labels = c("Center", "Left", "Right"),
#     values = c("red", "green", "blue"))



# now lets use ggThemeAssist to modify the graph to get all the factors changed


# Code to make your own theme -----
# here is an example of what I might do
theme_yourname <- function(base_size = 16, base_family = "Sans")
{
  theme(
    # plot inside panel stuff
    panel.background=element_rect(fill = "transparent", colour = "transparent"), 
    plot.background=element_rect(fill="transparent", colour=NA),
    # lines
    panel.grid.major = element_line(colour = NA, linetype = "blank"), 
    panel.grid.minor = element_line(colour = NA, linetype = "blank"), 
    
    # LABLES APPEARANCE
    axis.text = element_text(colour = "black"),
    axis.title.x=element_text(size=16, face="bold"),
    axis.title.y=element_text(size=16, face="bold"),
    axis.text.x = element_text(size=16, face="bold", angle=0, vjust = .5, hjust=.5),
    axis.text.y = element_text(size=16, face="bold"),
    
    # ADD AXES LINES AND SIZE
    axis.ticks = element_line(colour = "black"),
    axis.line.x = element_line(color="black", size = 0.5,linetype = "solid" ),
    axis.line.y = element_line(color="black", size = 0.5, linetype = "solid"),
    
    # LEGEND
    # legend stuff
    legend.position = "bottom", 
    legend.direction = "horizontal",
    
    legend.text = element_text(size = 14, face = "bold"), 
    legend.title = element_text(size = 14, face = "bold"), 
    legend.key = element_rect(fill = NA),
    legend.background = element_rect(fill = NA)
  )
}

# shorter version
# add + theme_yourname to the end of your plot summary



# what if we wanted to make this a data set that has the left right center
# as columns. Note that the distance from the central point differs slightly 
# a few decimal places out. 
# if we tried to match that it would not work well.
# so lets assume that we are close enough on a meter by meter scale.
# Lets round to the nearest meter - we could also do to the 10s if we wanted

plain.df <- plain.df %>% 
  mutate(distance_m = XXXX (distance_m, XXX))

# now lets make it by column or wide format
plain_wide.df <- plain.df %>% 
  pivot_wider(
    names_from = "XXXX",
    values_from = "XXXX"
  )

# note the names are a mess - lets fix that
plain_wide.df <- plain_wide.df %>% # what does the jantor do??

# what if we wanted to make it long again and get rid of na values
plain_long.df <- plain_wide.df %>% 
  pivot_longer(cols = c(x,y,z ),
               names_to = "XXXX",
               values_to = "XXXX")
  
  
  
  
  
  
  