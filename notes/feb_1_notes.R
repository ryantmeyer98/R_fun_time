
# INSTALL/LOAD LIBRARIES ----
# this code installs libraries if they are not yet installed, and loads them if they are
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(readxl)){install.packages("readxl")}
if(!require(skimr)){install.packages("skimr")}
if(!require(janitor)){install.packages("janitor")}

# READ IN THE DATA ----
# point and click method 
# import dataset (select the option for the file type you have)
# -> browse -> select file -> open -> import 

# code version
mm.df <- read_csv("Data/mms/mms.csv") %>%
  clean_names() %>%
  remove_empty(which = c("cols", "rows"))

# for an excel file
gpa.df <- read_excel("Data/gpa/gpa_mcat.xlsx") %>%
  clean_names() %>%
  remove_empty(which = c("cols", "rows"))

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
# lets say we want to calculate 
# base R approach
mm.df$sa <- ((mm.df$diameter)/2)^2 * pi

# tidyverse approach
mm.df <- mm.df %>%
  mutate(radius = diameter/2)

# what about SA - using tidyverse approach 
mm.df <- mm.df %>%
  mutate(sa = ((radius)/2)^2 * pi)

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


# TO MAKE PRETTY GRAPH - THE MEANING OF LIFE ----
# install.package("ggthemeassist")
# select the code you want to modify -> addins -> ggthemeassist -> make pretty graph :)






