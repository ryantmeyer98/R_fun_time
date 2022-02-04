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
left.df   <- read_csv("data/ryan_lange/Cottonwood_Marble_Canyon_L.csv") %>% clean_names()
right.df  <- read_csv("data/ryan_lange/Cottonwood_Marble_Canyon_R.csv") %>% clean_names()
center.df <- read_csv("data/ryan_lange/Cottonwood_Marble_Canyon_C.csv") %>% clean_names()

# note if you load these everything is capitalized and tough to type
# janitor can clean up the names - add    
#           %>% clean_names()
# now everything is fixed.

# making plots of elevation versus distance
left.df %>% 
  ggplot(aes(distance_m, elevation_m)) +
  geom_line()

# now how can we plot all three lines on one graph
# it is possible - but it is a pain - see this mess
  ggplot() + 
    geom_line(data=left.df, aes(x=distance_m, y=elevation_m), color="black")+
    geom_line(data=right.df, aes(x=distance_m, y=elevation_m), color="red")+
    geom_line(data=center.df, aes(x=distance_m, y=elevation_m), color="blue")

# Combine Files ---
# here we will bind the rows together top to bottom 
plain.df <- bind_rows(left.df, right.df, center.df)    
  
# now lets plot this
plain.df %>% 
  ggplot(aes(distance_m, elevation_m)) +
  geom_point()

# what is missing - some sort of mapping of shape or color to the name
plain.df %>% 
  ggplot(aes(distance_m, elevation_m, color=name)) +
  geom_line()

# now to add a layer of the labels of the graph
# command is labs=(x="stuff", y="stuff")
plain.df %>% 
  ggplot(aes(distance_m, elevation_m, color=name)) +
  geom_line()+
  labs(x="Distance (m)", y="Elevation (m)")

# Now to make the lines the color you want with new names?
plain.df %>% 
  ggplot(aes(distance_m, elevation_m, color=name)) +
  geom_line()+
  labs(x="Distance (m)", y="Elevation (m)") +
  scale_color_manual(
    name = "Position",
    labels = c("Center", "Left", "Right"),
    values = c("red", "green", "blue")
  )

# now lets use ggThemeAssist to modify the graph to get all the factors changed
plain.df %>% 
  ggplot(aes(distance_m, elevation_m, color=name)) +
  geom_line()+
  labs(x="Distance (m)", y="Elevation (m)") +
  scale_color_manual(
    name = "Position",
    labels = c("Center", "Left", "Right"),
    values = c("red", "green", "blue")) + 
  theme(
    # plot inside panel stuff
    panel.background=element_rect(fill = "transparent", colour = "transparent"), 
    plot.background=element_rect(fill="transparent", colour=NA),
    # lines
    panel.grid.major = element_line(colour = NA, linetype = "blank"), 
    panel.grid.minor = element_line(colour = NA, linetype = "blank"), 
    
    # LABLES APPEARANCE
    axis.text = element_text(colour = "black"),
    axis.title.x=element_text(size=13, face="bold"),
    axis.title.y=element_text(size=13, face="bold"),
    axis.text.x = element_text(size=12, face="bold", angle=0, vjust = .5, hjust=.5),
    axis.text.y = element_text(size=12, face="bold"),
    
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
    legend.background = element_rect(fill = NA))


# Code to make your own theme -----
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
plain.df %>% 
  ggplot(aes(distance_m, elevation_m, color=name)) +
  geom_line()+
  labs(x="Distance (m)", y="Elevation (m)") +
  scale_color_manual(
    name = "Position",
    labels = c("Center", "Left", "Right"),
    values = c("red", "green", "blue")) + 
  theme_yourname()


# what if we wanted to make this a data set that has the left right center
# as columns. Note that the distance from the central point differs slightly 
# a few decimal places out. 
# if we tried to match that it would not work well.
# so lets assume that we are close enough on a meter by meter scale.
# Lets round to the nearest meter - we could also do to the 10s if we wanted

plain.df <- plain.df %>% 
  mutate(distance_m = round(distance_m, 0))

# now lets make it by column or wide format
plain_wide.df <- plain.df %>% 
  pivot_wider(
    names_from = "name",
    values_from = "elevation_m"
  )

# note the names are a mess - lets fix that
plain_wide.df <- plain_wide.df %>% clean_names()

# what if we wanted to make it long again and get rid of na values
plain_long.df <- plain_wide.df %>% 
  pivot_longer(cols = c(cottonwood_marble_canyon_l, cottonwood_marble_canyon_r,
                        cottonwood_marble_canyon_c),
               names_to = "name",
               values_to = "elevation_m")
  
  
  
  
  
  
  