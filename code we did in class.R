# # # install packages for doing anova ----
# install.packages("car")
# install.packages("emmeans")
# install.packages("Hmisc")
# install.packages("multcompView")


# Load Libraries ----
library(tidyverse)
library(readxl)
library(patchwork)
library(janitor)

# Libraries for ANOVA ----
library(car)
library(emmeans)

# read in file
egg.df <- read_excel("data/emily_harders/Emily_Rfuntime_7Feb22.xlsx") %>% clean_names()


egg.df <- egg.df %>% 
  filter(sampling_day == 4) %>% # for rows
  select(sampling_day, treatment, acot13_normalized) # for columns

egg.df <- egg.df %>% 
  filter(sampling_day == 4) %>% # for rows
  select(-akr1d1_normalized, -cbr1_normalized,-fkbp51_normalized ) # for columns

egg.df %>% 
  ggplot(aes(x=treatment, y=acot13_normalized)) +
  geom_boxplot() +
  geom_point()

# set up anova
acot.lm <- lm(acot13_normalized ~ treatment, data = egg.df)

bartlett.test(acot13_normalized ~ treatment, data = egg.df)


qqPlot(acot.lm$residuals)

par(mfrow=c(2,2))
plot(acot.lm)

shapiro.test(acot.lm$residuals)

Anova(acot.lm, type="3")
summary(acot.lm)

acot.emm <- emmeans(acot.lm, ~ treatment)
plot(acot.emm, comparisons = TRUE)
acot.emmint <- emmeans(acot.emm, pairwise ~ treatment, adjust = "holm")
acot.emmint$emmeans
acot.emmint$contrasts

egg.df <- egg.df %>% 
  mutate(day = as.factor(sampling_day),
         treatment = as.factor(treatment))
