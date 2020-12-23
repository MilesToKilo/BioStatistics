# Outline -----------------------------------------------------------------

# 1. Load data with correct formatting
# 2. Normality assumption
# 3. Equal Variance assumption
# 4. Lack of fit assumption
# 5. Optimizing your code

# Load Data ---------------------------------------------------------------

# Load packages
pack <- c("tidyverse", # Main processing package
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

# View available datasets
library(help = "datasets")
# Let's use these datasets
head(warpbreaks) 
head(iris)

# Save as objects
warp <- as_tibble(warpbreaks) %>% # Convert from data frame to tibble 
  select(wool, tension, breaks) %>% # Change column order (preferrence)
  print() # Saving an object does not print the result, so this will
# Some functions require data frames, so we'll use as_data_frame()

# iris dataset has 3 different species, but we only want to compare 2
flowers <- as_tibble(datasets::iris) %>% 
  filter(Species == "setosa" | Species == "virginica") %>% # This OR that
  print()
# Avoid saving over the original dataset in case you need to start over

# Check that all variables are correctly identified
## i.e. wool is a categorical variable and should be listed as 
## a factor "<fct>"; this is important for graphing.
# If not, use readr functions like parse_factor().

# Get a grasp of data 
str(warp)
str(flowers)
warp %>% # Create pipe using this dataset
  group_by(wool, tension) %>% # Show different combinations
  summarise(count = n()) # Samples for each group

flowers %>% group_by(Species) %>% summarise(count = n())

# Why Check For Assumptions? ----------------------------------------------

## Statistical analyses require the appropriate tests to render accurate and 
## useful outputs. If assumptions are not checked and any statistical test
## is applied, the outputs will not be credible for dissemination. Using the
## incorrect test can lead to Type I errors (false positives) and Type II
## errors (false negatives), which is obviously not wanted. I am 
## sharing the assumptions addressed in the Biostatistics course I took, 
## which are essential for most of the tests that I've coded for. 

# Assumptions for Statistical Tests ---------------------------------------

## Parametric tests such as t-tests, ANOVAs, and linear regressions
## require checking for 2 assumptions before applying the correct test

# 1. Normality (normal distribution)
# 2. Equal variance (identically distributed)

## Normality can be checked with 2 subjective and 1 objective test. Subjective
## tests check the distribution of residuals in histograms and Q-Q plots while
## the objective test uses a Shapiro-Wilk Test.

# I'd recommend using all 3 tests to be confident in your answer. 

# Normality Assumption ----------------------------------------------------

# Example 1: Warpbreaks ---------------------------------------------------

# Research Question for 1st example: Does wool type affect the # of breaks?
# x-variable is wool (categorical); y-variable is breaks (continuous).

# First extract residuals (Deviation of each individual from the mean)
warp_model <- aov(breaks ~ wool, data = warp)
## Models change depending on your questions and tests. These examples focus
## on comparing 2 variables: x (categorical) to y(continuous)

warp_resid <- resid(warp_model) %>% 
  as_tibble() %>% # Needed for tidyverse functions
  print()

#### Subjective Tests: Histograms and Q-Q plots ####

# Produce a (basic) Histogram
warp_hist <- # Save graph as object; comment line to view graph
  warp_resid %>% ggplot(aes(x = value)) +
  labs(title = "Residuals", subtitle = "breaks ~ wool") +
  geom_histogram(binwidth = 3) + # Adjust binwidth according to data
  xlab("Residuals") + ylab("Frequency") + 
  scale_y_continuous(limits = c(0, 10), # Adjust to max y-value
                     breaks = seq(0, 10, by = 2)) # Adjust to max y-value
# Does the histogram look normally distributed? Is there a bell-shaped curve?
# Data is possibly skewed right (non-normal); double check with another test

# Produce a (basic) Q-Q plot
warp_qq <-
  warp_resid %>% 
  ggplot(aes(sample = rstandard(warp_model))) + 
  labs(title = "Q-Q Plot", subtitle = "breaks ~ wool") +
  geom_qq() + stat_qq_line() +
  ylab("Sample Quantities") + xlab("Theoretical Quantities")
## Are the values equally distributed from the predicted line? Is there no 
## evidence of a step-wise progression in values?
# Possibly not equally distributed (non-normal); triple check with another test 

#### Objective test: Shapiro-Wilk Test ####

# Function requires data frames; cannot use tibbles
shapiro.test(warp_resid) # Error!
warpbreak_resid <- resid(aov(breaks ~ wool, data = as_data_frame(warpbreaks)))
# If warning sign about "as_tibble", disregard
shapiro.test(warpbreak_resid)
# If p < 0.05, you have evidence of NON-normality.

# There's 3 options on how to proceed:
## 1. Transform your data to make a normal distribution
## 2. Perform a non-parametric test, which does not require normality
## 3. Perform a generalized linear model. 

# Some of these methods will be addressed in separate files, but not here.

# Example 2: Iris - Sepal Length ------------------------------------------

## Research question for 2nd example: Are the sepal lengths of setosa and
## virginica flowers morphologically different?
# x-variable is Species (categorical); y-variable is Sepal.Length (continuous).

# Extract residuals
flowers_modelSL <- aov(Sepal.Length ~ Species, data = flowers)
flowers_residSL <- resid(flowers_modelSL) %>% as_tibble()

# Plot Hisotgram
flowers_histSL <- # Comment this line to show graph, but save it as an object
  flowers_residSL %>% ggplot(aes(x = value)) +
  labs(title = "Residuals", subtitle = "Sepal Length ~ Species") +
  geom_histogram(binwidth = 0.1) + # Adjust binwidth according to data
  xlab("Residuals") + ylab("Frequency") + 
  scale_y_continuous(limits = c(0, 14), # Adjust to max y-value
                     breaks = seq(0, 14, by = 2)) # Adjust to max y-value
# Looks somewhat normally distributed; double check

# Plot Q-Q plot
flowers_qqSL <-
  flowers_residSL %>% 
  ggplot(aes(sample = rstandard(flowers_modelSL))) + 
  labs(title = "Q-Q Plot", subtitle = "Sepal Length ~ Species") +
  geom_qq() + stat_qq_line() +
  ylab("Sample Quantities") + xlab("Theoretical Quantities")
# Looks equally distributed; triple check

# Shapiro-Wilk Test
iris_resid <- resid(aov(Sepal.Length ~ Species, data = as_data_frame(flowers)))
shapiro.test(iris_resid)
# p-value > 0.05 shows evidence for normal distribution.

# Example 3: Iris - Sepal Width -------------------------------------------

## Research question for 3rd example: Are the sepal widths of setosa and
## virginica flowers morphologically different?
# x-variable is Species (categorical); y-variable is Sepal.Width (continuous).

# Extract residuals
flowers_modelSW <- aov(Sepal.Width  ~ Species, data = flowers) 
flowers_residSW <- resid(flowers_modelSW) %>% as_tibble()

# Plot Histogram
flowers_histSW <- 
  flowers_residSW %>% ggplot(aes(x = value)) +
  labs(title = "Residuals", subtitle = "Sepal Width ~ Species") +
  geom_histogram(binwidth = 0.1) + 
  xlab("Residuals") + ylab("Frequency") + 
  scale_y_continuous(limits = c(0, 22), breaks = seq(0, 22, by = 2)) 

# Plot Q-Q plot
# Suggests normally distributed; double check
flowers_qqSW <- flowers_residSW %>% 
  ggplot(aes(sample = rstandard(flowers_modelSW))) + 
  labs(title = "Q-Q Plot", subtitle = "Sepal Width ~ Species") +
  geom_qq() + stat_qq_line() +
  ylab("Sample Quantities") + xlab("Theoretical Quantities")
# Suggests equally distributed; triple check

# Shapiro-Wilk Test
iris_residSW <- resid(aov(Sepal.Width ~ Species, data = as_data_frame(flowers)))
shapiro.test(iris_residSW) 
# If p > 0.05, shows evidence for normal distribution

# Example 4: Iris - Petal Width -------------------------------------------

## Research question for 4th example: Are the petal lengths of setosa and
## virginica flowers morphologically different?
# x-variable is Species (categorical); y-variable is Petal.Length (continuous).

# Extract residuals
flowers_modelPL <- aov(Petal.Width  ~ Species, data = flowers) 
flowers_residPL <- resid(flowers_modelPL) %>% as_tibble() 

# Plot histogram
flowers_histPL <- 
  flowers_residPL %>% ggplot(aes(x = value)) +
  labs(title = "Residuals", subtitle = "Petal Length ~ Species") +
  geom_histogram(binwidth = 0.1) + 
  xlab("Residuals") + ylab("Frequency") + 
  scale_y_continuous(limits = c(0, 36), breaks = seq(0, 36, by = 2)) 
# Suggests non-normality; double check

# Plot Q-Q plot
flowers_qqPL <-
  flowers_residPL %>%
  ggplot(aes(sample = rstandard(flowers_modelPL))) + 
  labs(title = "Q-Q Plot", subtitle = "Petal Length ~ Species") +
  geom_qq() + stat_qq_line() +
  ylab("Sample Quantities") + xlab("Theoretical Quantities")
# Suggests non-normality; triple check

# Shapiro-Wilk Test
iris_residPL <- resid(aov(Petal.Length ~ Species, data = as_data_frame(flowers)))
shapiro.test(iris_residPL) # p < 0.05, NON-normal

# Equal Variance Assumption -----------------------------------------------

# Like previously, you have 3 options to check for equal variance:
## 2 subjective by comparing variances and standard deviations yourself or 
## 1 objective using the Bartlett Test or Fligner Test

#### Subjective Tests: Comparing sd() and var() ####

# First calculate the standard deviation and variance of y-variables
warp %>% group_by(wool) %>% 
  summarise(breaks_sd  = sd(breaks,  na.rm = TRUE), # Remove NA values
            breaks_var = var(breaks, na.rm = TRUE))
# For both var() and sd(), divide the largest by the smallest number
# You have evidence of unequal variances if:
# Variance ratio > 4 or standard deviation ratio > 2

# What do you conclude about the wool groups?
# Evidence for equal variances; double check

flowers %>% group_by(Species) %>% 
  summarise(Sepal.Length_sd   = sd(Sepal.Length, na.rm = TRUE),
            Sepal.Length_var  = var(Sepal.Length, na.rm = TRUE),
            Sepal.Width_sd   = sd(Sepal.Width, na.rm = TRUE),
            Sepal.Width_var  = var(Sepal.Width, na.rm = TRUE),
            Petal.Length_sd   = sd(Petal.Length, na.rm = TRUE),
            Petal.Length_var  = var(Petal.Length, na.rm = TRUE)) %>% 
  print()

# What do you conclude about ratios for var() and sd()?
# No evidence of un-equal variances for sepal length & width; double check
# Possible evidence of UN-equal variances for petal length; double check

#### Objective Test: Bartlett Test and Fligner Test ####

# Which test to use depends on the dataset's normality assumption
# If normal,     then apply Bartlett Test
# If non-normal, then apply Fligner Test

# Recap of assumptions:
# Warp and petal length are non-normal 
# Sepal length and width are normal

# Take note of the different outputs and how it could alter your interpretations
bartlett.test(data = warp, breaks ~ wool) # Incorrect test
fligner.test(data  = warp, breaks ~ wool) # Correct test
# If p > 0.05, you have evidence of equal variances

bartlett.test(data = flowers, Sepal.Length ~ Species) # UN-equal variances
bartlett.test(data = flowers, Sepal.Width  ~ Species) # Equal variances
fligner.test(data  = flowers, Petal.Length ~ Species) # UN-equal variances

## If you have unequal variances, you can apply transformations to your dataset.
## Typically used functions include: square root, Log, and Log10. Ultimately, 
## this will be a trial and error process. You will need to reference the 
## transformed dataset if you run statistics on it. 

# Lack of Fit -------------------------------------------------------------

## Linear regressions require you to check for lack of fit, which means,
## does your model fit the data. The models are algebraic equations and 
## require residual plots, but that's a whole topic to cover. 
## I will address this topic within linear regressions.

# Now What?? --------------------------------------------------------------

## Now that you know whether these assumptions were validated or broken,
## you can apply the correct "flavor" or test. I'll break down the steps to
## various tests in separate files. 

# Extra - Consolidate Graphs into a Single Plot ---------------------------

## If you are running multiple tests requiring multiple graphs to check 
## assumptions, use the package ggpubr to consolidate them into a single
## plot. This will help with sharing statistics and your interpretations

browseURL("http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/81-ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page/#annotate-the-arranged-figure")

# Plots produced:
warp_hist
flowers_histSL
flowers_histSW
flowers_histPL
warp_qq
flowers_qqSL
flowers_qqSW
flowers_qqPL

# Organize graphs into a plot
assumptions <- ggarrange( # List plots sequentially
  warp_hist, flowers_histSL, flowers_histSW, flowers_histPL, # Row 1
  warp_qq, flowers_qqSL, flowers_qqSW, flowers_qqPL, # Row 2
  ncol = 4, nrow = 2, # Organize graphs into columns and rows
  labels = c("A", "B", "C", "D", "E", "F", "G", "H"))
# Annotate plot and print
annotate_figure(
  assumptions, # Graphs organized with ggarrange()
  top = text_grob("Subjective Tests for Normality", # Plot title
                  color = "red", face = "bold", size = 22), # Aesthetics
  bottom = text_grob("plot by @MilesToKilo", # Caption
                     color = "blue", face = "italic", size = 10, # Aesthetics
                     x = 1, # Starting position of caption
                     hjust = 1.5)) # Horizontal adjustment

# Extra-Extra - Optimizing your code --------------------------------------

## Notice how calculating values for comparing variances across multiple 
## variables was repetitive. If you have a dataset that compares > 2 groups 
## and/or > 2 variables, you can automate the calculations. 
## Not necessary but time effective and helpful for organization!

# These threads helped me figure out this problem.
browseURL("https://stackoverflow.com/questions/64453699/how-to-apply-a-list-of-functions-using-tidyverse-and-get-back-a-column-for-each")
browseURL("https://stackoverflow.com/questions/12064202/apply-several-summary-functions-on-several-variables-by-group-in-one-call?rq=1")

as_tibble(iris) %>% group_by(Species) %>%
  summarise_all(funs( # Apply these functions to every column
    var, sd)) %>% # Calculate variance and standard deviation
  # End pipe here to see the output before calculating ratio
  select(where(is.numeric)) %>% # Disregard grouping x-variable(s)
  pivot_longer(everything()) %>% # Convert formatting from wide to long
  group_by(name) %>% # Group by y-variable(s)
  summarise_all(funs(max, min, ratio = max / min))
