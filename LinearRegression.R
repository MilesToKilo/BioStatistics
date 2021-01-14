# Outline -----------------------------------------------------------------

# 1. Scatterplot Matrix
# 2. Assumptions
# 3. Linear Regression (Linear model)
# 4. Interpretting Results
# 5. Linear Regression (Curved model)
# 6. Partial F-Test

# Load Data ---------------------------------------------------------------

# Load packages
pack <- c("tidyverse",    # Main processing package
          "broom",        # Tidying stat outputs
          "RColorBrewer") # Color palettes for graphing

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

# Load data
?swiss
fert <- as_tibble(swiss, rownames = 'Name') %>% # Import rownames into a column
  select(Name, Education, Examination, 
         Fertility, Infant.Mortality) %>% # Subset for example
  print()

# Visualize patterns in data with scatterplot matrix ----------------------

# Utilize function that adds histograms to the scatterplot matrix
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, 
       #col = "red", # Change if you want to add color 
       ...)
}

# View data as a scatterplot matrix
pairs(fert[, 2:5], # Variables being analyzed
      pch = 19, # symbol type
      diag.panel = panel.hist, # Insert histogram
      cex.labels = 2, # Font size 
      cex = 2) # Symbol size

# Click the zoom button for easier vierwing
# Variables are listed in a diagonal with the data shown as a histogram
# Each intersection shows a scatterplot for the associated variables 
# Each scatterplot is repeated twice with the axes switched

## We are using select variables from the swiss dataset to show different trends
## in the data and how to apply the correct the best-fitting formulas.

# Possible trends:
# Fertility x Infant Mortality - Linear (proportional changes)
# Education x Examination      - Square root (curves up towards a asymptote)
# Examination x Fertility      - Exponential decay (curves downward)
# Education x Infant Mortality - No trend

# Example 1: Fertility x Infant Mortality ---------------------------------

# Check Assumptions -------------------------------------------------------

## Process is described in a previous file, but there are some adjustments
## 1. Switch aov() with lm() 
## 2. Add arbitrary categorical variable for checking variances
## 3. Check for best fit line using a residual plot

# Check normality
## Although both variables are continuous and not necessarily affecting each other,
## you want to put the variable most likely to influence the other as the X-variable
fert_FxI_model <- lm(Infant.Mortality ~ Fertility, data = fert)
fert_FxI_resid <- resid(fert_FxI_model)
shapiro.test(fert_FxI_resid) # Normal Distribution

# Check variances
## To compare variances between 2 continuous variables, data needs to be 
## grouped into categories

fert %>% summarise(count = n(),
                   grouping = count/3) # Count for 3 equal groups
fert <- fert %>% arrange(Fertility) %>% # Organize data by x-variable
  mutate(Group_F = rep(c("A", "B", "C"), # Repeat group names
                     c(16, 16, 15))) %>% # Arbitrary groups
  print()

bartlett.test(Infant.Mortality ~ Group_F, data = fert) # Equal variances

# Check for best fit line

plot(fert$Fertility, fert_FxI_resid) %>% # Plot residuals along your x-variable
  abline(h = 0) # Flot a horizontal line at 0 for reference
# Are the residuals equally distributed around the horizontal line?
# I would argue yes, so the (linear) model is the best fit line

# Linear Regression -------------------------------------------------------

## This may be confusing, but you check your assumptions using lm(), and then
## you run your statistical analyses using aov(), and lastly, use your lm()
## to pull info like the y-intercept and slope of the best-fit line 
fert_FxT_aov <- aov(Infant.Mortality ~ Fertility, data = fert)

# Results for whole model
tidy(fert_FxT_aov)
# Results about fitted line
tidy(fert_FxI_model)

# Interpretting results ---------------------------------------------------

# Plotting your data will help you interpret your results
# Select a colorblind friendly palette
RColorBrewer::display.brewer.all(colorblindFriendly = TRUE) # View palettes

fert %>% 
  ggplot(aes(x = Fertility, y = Infant.Mortality)) +
  geom_point(aes(color = Group_F, size = 1)) +
  stat_smooth(method = "lm", color = "black", size = 2, 
              formula = y ~ x, # This needs to mirror your lm() formula
              se = FALSE) + # Toggle depending on your preference
  scale_fill_brewer(palette = "Dark2", aesthetics = "color") +
  scale_size(guide = "none")

# Results for whole model
tidy(fert_FxT_aov)
## Fertility has a significant effect on Infant Mortality (F = 9.45; df = 1, 45; 
## p = 0.00359). This pattern for identifying the appropriate Test Statistic, 
## df, and p-value remains true with multiple comparisons. Examples to follow.

# Results about fitted line
tidy(fert_FxI_model)

## y-intercept = 13.1; slope = 0.0971; 
## p-value for (intercept) = are the intercepts are different?
## You don't typically report these p-values b/c aov() accounts for the whole model

# Example 2: Education x Examination --------------------------------------

# Check Assumptions -------------------------------------------------------

# Check normality
fert_ExE_model <- lm(Examination ~ Education, data = fert)
fert_ExE_resid <- resid(fert_ExE_model)
shapiro.test(fert_ExE_resid) # Normal Distribution

# Check variances
# Create new grouping
fert <- fert %>% arrange(Education) %>% # Organize data by x-variable
  mutate(Group_E = rep(c("A", "B", "C"), # Repeat group names
                     c(16, 16, 15))) %>% # Arbitrary groups
  print()

bartlett.test(Examination ~ Group_E, data = fert) # Equal variances

# Check for best fit line

plot(fert$Education, fert_ExE_resid) %>% # Plot residuals along your x-variable
  abline(h = 0) # Flot a horizontal line at 0 for reference
# Are the residuals equally distributed around the horizontal line?
# I would argue no since the points are move heavily found below the line

# Adjusting your linear model

# Scatterplot patterns have different exponents to adjust their best fit line
# Linear:             x ^ 1 (does not need to be specified)
# Exponential growth: x ^ (# > 1)
# Exponential decay:  x ^ (# < 1)
# Square root:        x ^ (1 / 2)


# Adjust your linear model
fert_ExE_model2 <- lm(Examination ~ Education + 
                        I(Education ^ (1/2)), data = fert) # Follow this pattern
fert_ExE_resid2 <- resid(fert_ExE_model2)
shapiro.test(fert_ExE_resid2) # Normal distribution
bartlett.test(Examination ~ Group_E, data = fert) # Equal variances

# Graph residual plot
plot(fert$Education, fert_ExE_resid2) %>% abline(h = 0) 
# Points look more evenly distributed around the horizontal line

# Linear Regression -------------------------------------------------------

fert_ExE_aov <- aov(data = fert, Examination ~ Education + I(Education ^ (1/2)))

# Results for whole model
tidy(fert_ExE_aov)
# Results about fitted line
tidy(fert_ExE_model2)

# Interpretting Results ---------------------------------------------------

fert %>% 
  ggplot(aes(x = Education, y = Examination)) +
  geom_point(aes(color = Group_E, size = 1)) +
  stat_smooth(method = "lm", color = "black", size = 2,
              formula = y ~ x + I(x ^ (1 / 2)), se = FALSE) +
  stat_smooth(method = "lm", color = "red", size = 2, # Comment to hide
              formula = y ~ x, se = FALSE) + # Bad fitted line
  scale_fill_brewer(palette = "Dark2", aesthetics = "color") +
  scale_size(guide = "none")

# Results for whole model
tidy(fert_ExE_aov)
## Education shows a borderline significant curvilinear relationship with 
## Examinations (df = 1, 44; F = 3.42; p = 0.0711). 
# Results about fitted line
tidy(fert_ExE_model2)
# y-intercept: 1.53; (curvilinear) slope: 5.01

# Partial F-Test ----------------------------------------------------------

## If you want to double check whether your curved line is a better model than
## the linear model, you can run a partial F-test to compare the models. When
## reporting your overall results, you can report either your whole model
## results (aov()) or your partial F-test's results (anova())

# I know it's confusing, but use anova() for a partial F-test
anova(fert_ExE_model2, # Curved model
      fert_ExE_model) # Linear model

## These results mirror the aov() results that show this new model as borderline
## significant. I think you could actually report either the linear or curved
## model results and be okay. Just depends on how you interpret your data.
