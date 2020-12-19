# Outline -----------------------------------------------------------------

# 1. Load data with correct formatting
# 2. Normality assumption
# 3. Equal Variance assumption
# 4. Lack of fit assumption

# Load Data ---------------------------------------------------------------

# Load package
library(tidyverse) # Main processing package
# View available datasets
library(help = "datasets")
# Let's use these datasets
head(warpbreaks) 
head(iris)

# Save as objects
warp <- as_tibble(warpbreaks) %>% # Convert from data frame to tibble 
  select(wool, tension, breaks) %>% # Change column order
  print()
warpbreaks <- warpbreaks # Some functions require data frames

flowers <- as_tibble(iris) %>% select(Species, everything()) %>% print()
iris <- iris

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
## is applied, the outputs will not be credible for dissemination. I am 
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
warp_resid %>% ggplot(aes(x = value)) +
  labs(title = "Residuals", subtitle = "breaks ~ wool") +
  geom_histogram(binwidth = 3) + # Adjust binwidth according to data
  xlab("Residuals") + ylab("Frequency") + 
  scale_y_continuous(limits = c(0, 10), # Adjust to max y-value
                     breaks = seq(0, 10, by = 2)) # Adjust to max y-value
# Does the histogram look normally distributed? Is there a bell-shaped curve?
# Data is possibly skewed right (non-normal); double check with another test

# Produce a (basic) Q-Q plot
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
warpbreak_resid <- resid(aov(breaks ~ wool, data = warpbreaks))
shapiro.test(warpbreak_resid)
# If p < 0.05, you have evidence of NON-normality.

# There's 3 options on how to proceed:
## 1. Transform your data to make a normal distribution
## 2. Perform a non-parametric test, which does not require normality
## 3. Perform a generalized linear model. 

# Some of these methods will be addressed in separate files, but not here.

# Example 2: Iris ---------------------------------------------------------

## Research question for 2nd example: Are these flower species morphologically 
## different?
# x-variable is Species (categorical); y-variable is Sepal.Length (continuous).

# Extract residuals
flowers_model <- aov(Sepal.Length ~ Species, data = flowers)
flowers_resid <- resid(flowers_model) %>% as_tibble()

# Plot Hisotgram
flowers_resid %>% ggplot(aes(x = value)) +
  labs(title = "Residuals", subtitle = "Sepal Length ~ Species") +
  geom_histogram(binwidth = 0.1) + # Adjust binwidth according to data
  xlab("Residuals") + ylab("Frequency") + 
  scale_y_continuous(limits = c(0, 18), # Adjust to max y-value
                     breaks = seq(0, 18, by = 2)) # Adjust to max y-value
# Looks somewhat normally distributed; double check

# Plot Q-Q plot
flowers_resid %>% 
  ggplot(aes(sample = rstandard(flowers_model))) + 
  labs(title = "Q-Q Plot", subtitle = "Sepal Length ~ Species") +
  geom_qq() + stat_qq_line() +
  ylab("Sample Quantities") + xlab("Theoretical Quantities")
# Looks equally distributed; triple check

# Shapiro-Wilk Test
iris_resid <- resid(aov(Sepal.Length ~ Species, data = iris))
shapiro.test(iris_resid)
# p-value > 0.05 shows evidence for normal distribution.

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
            Sepal.Length_var  = var(Sepal.Length, na.rm = TRUE))
# What do you conclude about the flower species?
# Possible evidence for UN-equal variances; double check

#### Objective Test: Bartlett Test and Fligner Test ####

# Which test to use depends on the dataset's normality assumption
# If normal,     then apply Bartlett Test
# If non-normal, then apply Fligner Test

# Recall Warp is non-normal while flowers is normal
bartlett.test(data = warp, breaks ~ wool) # Incorrect test
fligner.test(data  = warp, breaks ~ wool) # Correct test
# If p > 0.05, you have evidence of equal variances

bartlett.test(data = flowers, Sepal.Length ~ Species) # Correct test
fligner.test(data  = flowers, Sepal.Length ~ Species) # Incorrect test
# If p < 0.05, you have evidence of UN-equal variances

# If you have unequal variances, you can apply transformations to correct it.
# Typically used functions include: square root, Log, and Log10.
# Ultimately, this will be a trial and error process.


# Lack of Fit -------------------------------------------------------------

## Linear regressions require you to check for lack of fit, which means,
## does your model fit the data. The models are algebraic equations and 
## require residual plots, but that's a whole topic to cover. 
## I will address this topic within linear regressions.

# Now What?? --------------------------------------------------------------

## Now that you know whether these assumptions were validated or broken,
## you can apply the correct "flavor" or test. I'll break down the steps to
## various tests in separate files. 