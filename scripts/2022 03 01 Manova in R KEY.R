# today we are doing a MANOVA
# the sites I have been reading for this are 
# Steve Juliano's notes from advanced biostats
# https://appsilon.com/manova-in-r/
# http://www.sthda.com/english/wiki/manova-test-in-r-multivariate-analysis-of-variance
# https://www.datanovia.com/en/lessons/one-way-manova-in-r/
# https://www.reneshbedre.com/blog/manova.html
# http://dwoll.de/rexrepos/posts/multMANOVA.html
# https://tjmurphy.github.io/jabstb/manova.html
# 

# # install these packages
# install.packages("gridExtra")
# install.packages("effectsize")
# install.packages("MASS")
# install.packages("ggpubr") - some cool stuff 
# # https://rpkgs.datanovia.com/ggpubr/reference/ggqqplot.html
# install.packages("MVN")
# # https://cran.r-project.org/web/packages/MVN/MVN.pdf
# install.packages("rstatix")
# # https://www.datanovia.com/en/lessons/one-way-manova-in-r/
# install.packages("GGally")
# install.packages("broom")



# libraries
library(gridExtra )
library(effectsize) # use for partial eta squared effect size
library(MASS) # LDA of the MANOVA # note the issues with DPLYR
library(MVN) # multivariate normality
library(ggpubr) # some super cool graphs to use later on as well
library(car)
library(rstatix) # some of the multicollinearity test
library(GGally) 
library(broom)
library(janitor)
library(tidyverse)
library(patchwork)


# Manova is like an ANOVA but with mutliple response variables
# Because MANOVA uses more than one dependent variable, the null and 
# the alternative hypotheses are slightly changed:
#   
#   H0: Group mean vectors are the same for all groups 
#       or they don’t differ significantly.
#   H1: At least one of the group mean vectors is different from the rest.

# MANOVA in R won’t tell you which group differs from the rest, but that’s 
# easy to determine via a post-hoc test. We’ll use 
# Linear Discriminant Analysis (LDA) to answer this question later. 
# You can also use univarite ANOVAs on each response variable. This is what 
# you will see most often but is not the best thing to do as it throws the 
# power of the MANOVA out the window.

# MANOVA statistical test has many strict assumptions. 
# The ones from ANOVA carry over – 
# independence of observations and homogeneity of variances 
# an also include the following:
  
# MANOVA makes the following assumptions about the data:
  
# # Adequate sample size. Rule of thumb: the n in each cell > the number 
#           of outcome variables.
# # Independence of the observations. Each subject should belong to only one group. 
#       There is no relationship between the observations in each group. 
#       Having repeated measures for the same participants is not allowed. 
#       The selection of the sample should be completely random.
# # Absence of univariate or multivariate outliers.
# # Multivariate normality. The R function mshapiro_test( )[in the rstatix package] 
#            can be used to perform the Shapiro-Wilk test for multivariate normality.
# # Absence of multicollinearity. 
#            The dependent (outcome) variables cannot be too correlated 
#             to each other. No correlation should be above r = 0.90 [Tabachnick and Fidell (2012)}.
# # Linearity between all outcome variables for each group.
# # Homogeneity of variances. The Levene’s test can be used to test the 
#          equality of variances between groups. 
#         Non-significant Levene’s test indicate equal variance between groups.
# # Homogeneity of variance-covariance matrices. The Box’s M Test can be used 
#         to check the equality of covariance between the groups. 
#         This is the equivalent of a multivariate homogeneity of variance. 
#         This test is considered as highly sensitive. Therefore, significance 
#         for this test is determined at alpha = 0.001.

# the approach - we will use a dataset that is built into R for the demo
# this is the iris dataframe and is part of R
# Read in dataferame ----
iris.df <- iris %>% clean_names()

# lets make it long as well
# convert to a long format
iris_long.df <- iris.df %>% 
  pivot_longer(cols = -"species", names_to = "variable", values_to = "value") %>% 
  group_by(species, variable) 

# Plots -----
# Boxplots of wide data ----
# we will use 
box_sl <- ggplot(iris.df, aes(x = species, y = sepal_length, fill = species)) +
  geom_boxplot() +
  theme(legend.position = "bottom")
box_sw <- ggplot(iris.df, aes(x = species, y = sepal_width, fill = species)) +
  geom_boxplot() +
  theme(legend.position = "bottom")
box_pl <- ggplot(iris.df, aes(x = species, y = petal_length, fill = species)) +
  geom_boxplot() +
  theme(legend.position = "bottom")
box_pw <- ggplot(iris.df, aes(x = species, y = petal_width, fill = species)) +
  geom_boxplot() +
  theme(legend.position = "bottom")

# Using patchwork
box_sl + theme(axis.title.x = element_blank(),
               axis.text.x = element_blank()) + 
  box_sw + theme(axis.title.x = element_blank(),
                 axis.text.x = element_blank()) + 
  box_pl + 
  box_pw + 
  plot_layout(ncol=2,  guides = "collect" ) &
  theme(legend.position='bottom')

# # using Grid arrange
# grid.arrange(box_sl, box_sw, box_pl, box_pw, ncol = 2, nrow = 2)

# Box plot of data ----
iris_long.df %>% 
  ggplot(aes(species, value, fill=species)) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free_y")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# assumptions at bottom

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# One-way MANOVA in R using car ------
# We can now perform a one-way MANOVA in R. 

manova.model <- lm(cbind(sepal_length, sepal_width, petal_length, petal_width) ~ species, iris.df)

Manova(manova.model, test.statistic = "Pillai",  type="III")

summary(manova.model, multivariate=TRUE)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Univariate tests ------

summary(Anova(manova.model), univariate=TRUE, multivariate=FALSE,
        p.adjust.method=TRUE)

# Univariate ANOVAS ----
# Look to see which differ
summary.aov(manova.model)


# Do welch one way anova test
iris_long.df %>% group_by(variable) %>% welch_anova_test(value ~ species)
# or do Kruskal-Wallis test
iris_long.df %>% group_by(variable) %>% kruskal_test(value ~ species)
# or use aov()
iris_long.df %>% group_by(variable) %>% anova_test(value ~ species)

# Visulaize univariate results -----
iris_long.df %>% 
  ggplot(aes(species, value))+
  geom_jitter(width=0.1)+
  stat_summary(fun = mean, colour="red", 
               geom="line", aes(group = 1)) +
  stat_summary(fun = mean, colour="red", 
               geom="point", size = 3, aes(group = 1)) +
  stat_summary(fun.data = mean_se, colour="red", 
               geom="errorbar", size = 1, width = 0.2, aes(group = 1)) +
  facet_wrap(~variable, scales="free")

# Print the linear model results to view the effect sizes as regression coefficients:
manova.model

# We’re usually not interested in the information on the intercept term or 
# the statistical test for it. It is the group mean for the control condition, 
# in this case, demonstrating the background level of clearance. 
# Whether that differs from zero not important to know.
tidy(manova.model)

# Therefore to make the p-value adjustment we should only be interested in 
# the estimates for the ‘species’ terms. 
# We shouldn’t waste precious alpha on the intercept values.
# Here’s how we do that:
 tidy(manova.model) %>% 
  select(term, response, estimate, p.value) %>% 
  # filter(row_number() %% 2 == 0) %>%
  filter(term != "(Intercept)") %>% 
  mutate(p.value_adjusted = p.adjust(p.value, 
                                   method="bonferroni"))
  
# Note that, as we have two dependent variables, we need to apply 
# Bonferroni multiple testing correction by decreasing the he level we 
# declare statistical significance.

# This is done by dividing classic alpha level (0.05) by the number of tests 
# (or dependent variables, here 2). This leads to a significance acceptance 
# criteria of p < 0.025 rather than p < 0.05 because there are two dependent variables.

# Compute multiple pairwise comparisons
# A statistically significant univariate ANOVA can be followed up by 
# multiple pairwise comparisons to determine which groups are different.

# The R functions tukey_hsd() [rstatix package] can be used to compute 
# Tukey post-hoc tests if the homogeneity of variance assumption is met.

# If you had violated the assumption of homogeneity of variances, 
# as in our example, you might prefer to run a Games-Howell post-hoc test. 
# It’s also possible to use the function pairwise_t_test() [rstatix] 
# with the option pool.sd = FALSE and var.equal = FALSE .

pwc <- iris_long.df %>%
  # group_by(species) %>% 
  games_howell_test(value ~ species) %>%
  select(-estimate, -conf.low, -conf.high) # Remove details
pwc


# Estimate the effect size. 
# One metric often used with MANOVA is Partial Eta Squared. 
# It measures the effect the independent variable has on the dependent variables. 
# If the value is 0.14 or greater, we can say the effect size is large. 

effectsize::eta_squared(manova.model)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Multivaraite Post-Hoc Test ------
# The P-Value is practically zero, and the Partial Eta Squared suggests a 
# large effect size – but which group or groups are different from the rest? 
# There’s no way to tell without a post-hoc test. We’ll use
# Linear Discriminant Analysis (LDA), which finds a linear combination of 
# features that best separates two or more groups.

# By doing so, we’ll be able to visualize a scatter plot showing the two 
# linear discriminants on the X and Y axes, and color code them to match 
# our independent variable – the flower species.

# The best practice is to separate the dependent from the independent variable 
# before calling the manova() function. 
# Once the test is done, you can print its summary:

# this is one way of setting this up
dependent_vars <- cbind(iris.df$sepal_length, iris.df$sepal_width, 
                        iris.df$petal_length, iris.df$petal_width)
independent_var <- iris.df$species

iris.lda <- lda(independent_var ~ dependent_vars, CV = F)
iris.lda

# Look at  coefficients to see how dependent variables are used to form 
# the LDA decision rule.
# LD1 is calculated as 
# LD1 = 0.83 * Sepal.Length + 1.53 * Sepal.Width - 2.20 * Petal Length - 
#       2.81 * petal_width

# The snippet below uses the predict() function to get the linear discriminants 
# and combines them with our independent variable:

lda.df <- data.frame(
  species = iris.df[, "species"],
  lda = predict(iris.lda)$x)

head(lda.df)

# now the scatter plot
ggplot(lda.df) +
  geom_point(aes(x = lda.LD1, y = lda.LD2, color = species), size = 4) +
  theme_classic()



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Assumptions ------

# Check sample size assumption
# more samples per independent varaible than dependent variables
iris.df %>%
  group_by(species) %>%
  summarise(N = n())

# No univariate outliers outliers  -----
iris.df %>%
  group_by(species) %>%
  identify_outliers(sepal_length)

iris.df %>%
  group_by(species) %>%
  identify_outliers(sepal_width)

iris.df %>%
  group_by(species) %>%
  identify_outliers(petal_length)

iris.df %>%
  group_by(species) %>%
  identify_outliers(petal_width)

# not easier if in long format
iris_long.df %>% 
  group_by(species, variable) %>% 
  identify_outliers(value)

# Detect multivariate outliers-----
# Multivariate outliers are data points that have an unusual combination of 
# values on the outcome (or dependent) variables. 
# In MANOVA setting, the Mahalanobis distance is generally used to detect 
# multivariate outliers. The distance tells us how far an observation is 
# from the center of the cloud, taking into account the shape (covariance) 
# of the cloud as well.

# The function mahalanobis_distance() [rstatix package] can be easily 
# used to compute the Mahalanobis distance and to flag multivariate outliers. 
# Read more in the documentation of the function.

# This metric needs to be calculated by groups:
# Compute distance by groups and filter outliers
# Use -id to omit the id column in the computation
iris.df %>%
  group_by(species) %>%
  mahalanobis_distance() %>%
  filter(is.outlier == TRUE) %>%
  as.data.frame()

# there are none

# Check univariate normality assumption -----
# The normality assumption can be checked by computing Shapiro-Wilk test 
# for each outcome variable at each level of the grouping variable. 
# If the data is normally distributed, the p-value should be less than 0.05.
iris.df %>%
  group_by(species) %>%
  shapiro_test(sepal_length, sepal_width, petal_length, petal_width) %>%
  arrange(variable)

# another way to do qqplots
# QQ plot of Sepal.Length
# this uses ggpubr - 
ggqqplot(iris.df, "Sepal.Length", facet.by = "species",
         ylab = "Sepal Length", ggtheme = theme_bw())

ggqqplot(iris.df, "Sepal.Width", facet.by = "species",
         ylab = "Sepal Sepal.Width", ggtheme = theme_bw())

ggqqplot(iris.df, "petal_length", facet.by = "species",
         ylab = "petal_length Length", ggtheme = theme_bw())

ggqqplot(iris.df, "petal_width", facet.by = "species",
         ylab = "petal_width", ggtheme = theme_bw())

# Multivariate normality ----
# https://cran.r-project.org/web/packages/MVN/MVN.pdf
result = mvn(data = iris.df[-4], subset = "species", mvnTest = "hz",
             univariateTest = "AD", univariatePlot = "histogram",
             multivariatePlot = "qq", multivariateOutlierMethod = "adj",
             showOutliers = TRUE, showNewData = TRUE)


#### Multivariate Normality Result
result$multivariateNormality

### Univariate Normality Result
result$univariateNormality

### Descriptives
result$Descriptives

# ### Multivariate Outliers
# result$multivariateOutliers
# ### New data without multivariate outliers
# result$newData

# Another way for Multivariate normality
iris.df %>%
  select(sepal_length, sepal_width, petal_length, petal_width) %>%
  mshapiro_test()
# close 

# Multicollinearity ----
# To check for multicollinearity problem in our model, we need the vif() function 
# from the car package in R. VIF stands for variance inflation factor.
# CIt measures how much the variance of any one of the coefficients is 
# inflated due to multicollinearity in the overall model.

# As a rule of thumb, a vif score over 5 is a problem. 
# A score over 10 should be remedied and you should consider dropping the 
# problematic variable from the regression model or creating an index of all 
# the closely related variables.
# need to work on this
# # https://rforpoliticalscience.com/2020/08/03/check-for-multicollinearity-with-the-car-package-in-r/
# summary(linear <- lm(species ~ Sepal.Length + Sepal.Width + petal_length + petal_width, data = iris.df))
# car::vif(manova_model)


# # Multicolinearity for two ----
iris.df %>% cor_test(sepal_length, petal_length)

# # Multicolinearity for > two ----
# A correlation above 0.9 is an indication of multicollinearity, 
# which is problematic for MANOVA.
iris.df %>% cor_mat(sepal_length, petal_length, sepal_width, petal_width)


# Check linearity assumption -----
# The pairwise relationship between the outcome variables should be linear 
# for each group. This can be checked visually by creating a scatter plot 
# matrix using the R function ggpairs() [GGally package]. 
linearity.plots <- iris.df %>%
  select(sepal_length, petal_length, sepal_width, petal_width, species) %>%
  group_by(species) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
linearity.plots

# Show the plots
linearity.plots$plots

# Homogeneity of covariances -----
# This can be evaluated using the Box’s M-test implemented in the rstatix package.
# https://www.datanovia.com/en/lessons/one-way-manova-in-r/
# if balanced and significant not a big deal
box_m(iris.df[, c("sepal_length", "petal_length", "sepal_width", "petal_width")], iris.df$species)

# Homogeneity of variance assumption
# For each of the outcome variables, the one-way MANOVA assumes that there 
# are equal variances between groups. 
# \This can be checked using the Levene’s test of equality of variances. 
# Key R function: levene_test() [rstatix package].
iris_long.df %>% 
  group_by(variable) %>%
  levene_test(value ~ species)
