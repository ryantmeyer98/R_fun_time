# Packages you likely already have installed, but in case you don't -----
# Packages already installed
# install.packages("tidyverse")
# install.packages("readxl")

# # Packages to install and run library one time
# install.packages("ggThemeAssist")
# install.packages("styler")
# 
# # install packages in case you did not get them
# install.packages("skimr")
# install.packages("janitor")
# install.packages("patchwork")

# on gg theme assist 
# to use highlight the code you want to grach and select ggtheme assist from addins
# addins can be found at the top center of the screen

# Load these libraries only once, like ever, until you reinstall R ----
library(ggThemeAssist)
library(styler)

# load Libraries that we use in all scripts -----
library(tidyverse)
library(readxl)
library(skimr)
library(janitor)
library(patchwork)

# Read in the data 
# do for left
left.df   <- read_csv("data/ryan_lange/Cottonwood_Marble_Canyon_L.csv") %>% clean_names()
# right
right.df  <- read_csv("data/ryan_lange/Cottonwood_Marble_Canyon_R.csv") %>% clean_names()
# center
center.df <- read_csv("data/ryan_lange/Cottonwood_Marble_Canyon_C.csv") %>% clean_names()

# note if you load these everything is capitalized and tough to type
# janitor can clean up the names - add    
#           %>% clean_names()

# making plots of elevation versus distance
left.df %>% 
  ggplot(aes(x= distance_m, y= elevation_m)) +
  geom_line()

# if you have several data frame you want on one plot this is a way to do it,
# but it is generally not a good thing to do as there are far better ways to do it 
ggplot() + 
  geom_line(data=left.df, aes(x=distance_m, y=elevation_m), color="black") +
  geom_line(data = center.df, aes(x=distance_m, y=elevation_m), color="blue") +
  geom_line(data = right.df, aes(x=distance_m, y=elevation_m), color="red")

# Combine Files ---
# here we will bind the rows together top to bottom using bind_rows(dataframes to bind)
plain.df <- bind_rows(left.df, right.df, center.df)
  
# now lets plot this
# to add a different color for each line you add "color = "term from the model"" and it will
# provide a color for each line 
# same thing with line type 
  plain.df %>%
    ggplot(aes(x = distance_m, y = elevation_m, color = name, linetype = name)) +
    geom_line()
  
# what if we want to define what color the line is?
# can google ggplot colors to get the color codes 
  
# in this code we haven't specified the legent so it gives us two separate legends 
  plain.df %>%
    ggplot(aes(x = distance_m, y = elevation_m, color = name, linetype = name)) +
    geom_line() +
      scale_color_manual(
        name = "Name of legend title",
        labels = c("Center", "Left", "Right"),
        values = c("red", "green", "blue"))

  # now to add a layer of the labels of the graph
  # command is labs=(x="stuff", y="stuff")
  # + labs(x="",  y="")

# now lets use ggThemeAssist to modify the graph to get all the factors changed
# using ggtheme assist, highlight the code you want to work with,
# select ggthemeassist from addins menu


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
  mutate(distance_m = round (distance_m, 0))

# now lets make it by column or wide format
plain_wide.df <- plain.df %>% 
  pivot_wider(
    names_from = "name",
    values_from = "elevation_m"
  )

# note the names are a mess - lets fix that
plain_wide.df <- plain_wide.df %>% # what does the jantor do??
  
  # what if we wanted to make it long again and get rid of na values
  plain_long.df <- plain_wide.df %>% 
  pivot_longer(cols = c(x,y,z ),
               names_to = "XXXX",
               values_to = "XXXX")






