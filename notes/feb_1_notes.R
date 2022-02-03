
# INSTALL/LOAD LIBRARIES ----
# this code installs libraries if they are not yet installed, and loads them if they are
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(readxl)){install.packages("readxl")}
if(!require(skimr)){install.packages("skimr")}
if(!require(janitor)){install.packages("janitor")}

# READ IN THE DATA ----
# point and click method 
# import dataset -> browse -> select file -> open -> import 

# import csv
mm.df <- read_csv("Data/mms/mms.csv") %>%
  clean_names() %>%
  remove_empty(which = c("cols", "rows"))

# import excel file
gpa.df <- read_excel("Data/gpa/gpa_mcat.xlsx") %>%
  clean_names() %>%
  remove_empty(which = c("cols", "rows"))

# clean_names() = formats column headers to be in a format that works with R
# remove_empty(which =c("cols", "rows")) = removes empty columns and rows

# VIEWING DATA ----
# view the whole dataset
mm.df
# view the top of the dataset
head(mm.df)
# view the bottom of the dataset
tail(mm.df)
# view just the diameter
mm.df$diameter

# MUTATIONS ----
# mutate = modify an existing column or create a new column
# base R approach
mm.df$sa <- ((mm.df$diameter)/2)^2 * pi

# tidyverse approach :)
mm.df <- mm.df %>%
  mutate(radius = diameter/2)

# what about SA - using tidyverse approach 
mm.df <- mm.df %>%
  mutate(sa = ((radius)/2)^2 * pi)

# how this works
# mm.df <- mm.df %>% = pull the data from mm.df and save changes to mm.df then
# mutate(radius = diameter/2) = use the function mutate, create a new column called radius,
# radius is made by taking column diameter and dividing each cell by 2

# PLOTTING ----
# to make plot - scatter plot
ggplot(data = mm.df, aes(x = diameter, y = mass)) +
  geom_point()

# violin plot
ggplot(data = mm.df, aes(x = center, y = mass)) +
  geom_violin()

# what if we want to add color, fill?
ggplot(data = mm.df, aes(x = center, y = mass, color = color, fill = color)) +
  geom_violin()

# how about we add a scatter plot to our violin plot?
ggplot(data = mm.df, aes(x = center, y = mass)) +
  geom_violin() +
  geom_jitter()

# another way to jitter data
ggplot(data = mm.df, aes(x = center, y = mass)) +
  geom_violin() +
  geom_point(color = "blue", position = position_dodge2(width = .2))

# bar graph
ggplot(data = mm.df, aes(x = center, y = mass)) +
  geom_bar(stat = "identity")

# mean and standard error plot, we will add standard error lines in the future - ryan needs to fix this
ggplot(data = mm.df, aes(x = center, y = mass, color = color)) +
  stat_summary(fun=mean, na.rm=TRUE, geom = "point", position = position_dodge2(width = 1)) +
  stat_summary(fun.data = mean_se, na.rm = TRUE, geom = "errorbar", position = position_dodge2(width = 1))

# GENERAL GGPLOT SYNTAX ---
ggplot(data = data.df, mapping = aes(x = x, y = y)) +
  geom_geom()

# ggplots calls the function that plots, data says where the data is coming from
# mapping = aes(x, y) says here are the x and y variables to plot 
# + the plus means expect another row of information
# to actually make the plot you need to add a geometry, this is what geom_geom() does
# geom_geom() is just an example, some real examples are geom_point(), geom_jitter()

# shorthand for ggplot that Bill often uses
ggplot(data.df, aes(x,y)) +
  geom

# another way to write ggplot
data.df %>%
  ggplot(aes(x, y)) +
  geom_geom()






