# Outline -----------------------------------------------------------------

# 1. Check Assumptions
# 2. t-Tests
# 3. Graphs

# Load Data ---------------------------------------------------------------

# Load packages
pack <- c("tidyverse", # Main processing package
          "broom", # Tidying stat outputs
          "ggpubr") # Grouping plots


package.check <- lapply(
  pack, # List of packages to load
  FUN = function(x) { # Making a function
    if (!require(x, character.only = TRUE)) { # If you can't find the package
      install.packages(x, dependencies = TRUE) # First install it
      library(x, character.only = TRUE) # Then load it
    }
  }
)
rm(list = ls()) # Remove object and function after use

# Load Data ---------------------------------------------------------------

# Load datasets used in assumptions file
warp <- as_tibble(warpbreaks) %>% print()
# View the different groups
warp %>% group_by(wool, tension) %>% 
  summarise(count = n()) 
# iris dataset has 3 different species, but we only want to compare 2
flowers <- as_tibble(datasets::iris) %>% 
  filter(Species == "setosa" | Species == "virginica") %>% # This OR that
  print()
# Avoid saving over the original dataset in case you need to start over
# View the different groups
flowers %>% group_by(Species) %>% 
  summarise(count = n())

# Requirements ------------------------------------------------------------

## t-Tests require the x-variable to be independent and categorical with 
## 2-levels. The y-variable needs to be dependent and continuous.

# To compare 2 or more groups, you can use ANOVAs (described in another file).

## Applying the appropriate t-test depends on the dataset's normality
## and variance. With the 4 possible combinations are 4 different flavors of tests.

# Assumptions -------------------------------------------------------------

## The steps to checking assumptions were described in another file, so
## I'll provide the code for the objective tests with minimal explanation here:

#### Example 1: Warpbreaks ####
warpbreak_resid <- resid(aov(breaks ~ wool, data = as_data_frame(warp))) 
# If warning sign about "as_tibble", disregard
shapiro.test(warpbreak_resid) # If p < 0.05, data is NON-normal
fligner.test(data  = warp, breaks ~ wool) # If p> 0.05, data has = variance

# Conclusion: Warp is non-normal and has equal variances

#### Example 2: iris - Sepal Length ####
iris_residSL <- resid(aov(Sepal.Length ~ Species, data = as_data_frame(flowers)))
shapiro.test(iris_residSL) # p > 0.05, data is normal 
bartlett.test(data = flowers, Sepal.Length ~ Species) # p < 0.05, UN-equal variance

# Conclusion: iris - Sepal Length is normal and has unequal variances. 

#### Example 3: iris - Sepal Width
iris_residSW <- resid(aov(Sepal.Width ~ Species, data = as_data_frame(flowers)))
shapiro.test(iris_residSW) # p > 0.05, normal dsitribution
bartlett.test(data = flowers, Sepal.Width  ~ Species) # p > 0.05, equal var

# Conclusion: iris - sepal width is normal and has equal variances.

#### Example 4: iris - Petal Length ####

iris_residPL <- resid(aov(Petal.Length ~ Species, data = as_data_frame(flowers)))
shapiro.test(iris_residPL) # p < 0.05, NON-normal
bartlett.test(data = flowers, Petal.Length ~ Species) # p < 0.05, UN-equal

# Conclusion: iris - Petal Length is non-normal and has un-equal variances.

# Applying the Correct t-Test ---------------------------------------------

## Remember, the x-variable needs to be categorical with 2 levels while the 
## y-variable needs to be continous
# Format for function: t.test(y ~ x)

#### 1st. Proper Syntax ####

# First, I'll explain the correct syntax to have the code run
# This is a very helpful thread that uses t.test() with tidyverse functions
browseURL("https://stackoverflow.com/questions/50036411/r-running-a-t-test-from-piping-operators")

# The different options for using t.test() within a pipe ( %>% )
# 1. Place it inside a list
warp %>%
  summarise(ttest = list(t.test(breaks ~ wool, var.equal = TRUE))) 
# Extract only the p-value
warp %>% 
  summarise(pval = t.test(breaks ~ wool, var.equal = TRUE)$p.value)
# 2. Place within {}
warp %>%
  {t.test(.$breaks ~ .$wool, var.equal = TRUE)}
# 3. Specify the data (again)
warp %>%
  t.test(breaks ~ wool, data = ., var.equal = TRUE)

## I actually prefer to not use t.test() or other statistical tests within a 
## pipe, so the following is my preferred way to run a test
t.test(breaks ~ wool, var.equal = TRUE, data = warp)

#### 2nd. Applying the Correct Test ####

# 4 different flavors of t-tests from possible combinations of assumptions
# Recap of assumptions:
# iris - sepal width is normal and has equal variances.
# Warp is non-normal and has equal variances
# iris - Sepal Length is normal and has unequal variances. 
# iris - Petal Length is non-normal and has un-equal variances.

# If data is both normal and have equal variances: 
t.test(data = flowers, Sepal.Width ~ Species, var.equal = TRUE) # Sig. Diff!
# If data is non-normal, but equal variances: Kruskal-Wallis Test
kruskal.test(data = warp, breaks ~ wool) # Not Sig. Diff!
# If data is normal, but un-equal variances: Welch's t-Test
t.test(data = flowers, Sepal.Length ~ Species, var.equal = FALSE) # Sig. Diff!
# If data is both abnormal and unequal variances: Welch's t-Test
t.test(data = flowers, Petal.Width ~ Species, var.equal = FALSE) # Sig. Diff!
## There might be a better test for this last combination, but I was taught
## that this test would suffice. 

# If you'd like an output within a tibble, use broom::tidy()
tidy(t.test(data = flowers, Sepal.Width ~ Species, var.equal = TRUE)) %>% 
  select(statistic, # Select the values you need (it not all of them)
         parameter, p.value, method) %>% # Values most important (for me...)
  rename(df = parameter)
tidy(kruskal.test(data = warp, breaks ~ wool)) %>% 
  rename(df = parameter)
tidy(t.test(data = flowers, Sepal.Length ~ Species, var.equal = FALSE)) %>% 
  select(statistic, parameter, p.value, method) %>% 
  rename(df = parameter)
tidy(t.test(data = flowers, Petal.Width ~ Species, var.equal = FALSE)) %>% 
  select(statistic, parameter, p.value, method) %>% 
  rename(df = parameter)

# Graphing ----------------------------------------------------------------

## You can choose many routes to visualize your data. I will share 2 options
## that I typically use: Bar plots and violin plots.

# Graph Bar Plots ---------------------------------------------------------

# Bar plots help visualize the averages and standard error of each group.

# First, calculate summary statistics 
warp_sum <- warp %>% group_by(wool) %>% 
  summarise(
    breaks_mean  = mean(breaks),
    breaks_error = sd(breaks) / sqrt(n())) %>% 
  print()

# Graph (basic) bar plot
warp_sum %>% ggplot(aes(x = wool, y = breaks_mean, fill = wool)) + 
  geom_bar(stat = "identity") + # Bar height = mean value
  geom_errorbar(aes(ymin = breaks_mean + breaks_error, # Top error bar
                    ymax = breaks_mean - breaks_error), # Bottom error bar
                width = 0.35, lwd = 1) # Bolding error bar
## Although averages and standard errors are important, the graph does not 
## illustrate how the dataset distributions differ. I think knowing the
## distribution would be more revealing or helpful when comparing groups

# Graph Violin Plot -------------------------------------------------------

## Violin plots are more descriptive about the distribution of values although
## it does lack averages and standard error values. I address the shortcoming
## by overlapping the mean as a point.

# Violin plots combine a boxplot and normal distribution
browseURL("https://towardsdatascience.com/understanding-boxplots-5e2df7bcbd51")

# Graph (basic) violin plot
warp %>% 
  ggplot(aes(x = wool, y = breaks, fill = wool)) +
  geom_violin(size = 1, color = "black", # Adjusting outline 
              alpha = 0.6) + # Increasing transparency to offset boxplot
  geom_boxplot(width = 0.1, size = 1, color = "black") + # Adjusting outline
  stat_summary(geom = "point", color = "white", # Plot a white point
               fun = "mean") # Calculate the mean for the point
