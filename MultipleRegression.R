# Outline -----------------------------------------------------------------

# 1. Scatterplot Matrix
# 2. Assumptions
# 3. Multiple Regression (2 groups)
# 4. Multiple Regression (3 groups)
# 5. Partial F-Test
# 6. 3D Scatterplot (comparing 3 continuous variables)

# Load Data ---------------------------------------------------------------

# Load packages
pack <- c("tidyverse",    # Main processing package
          "broom",        # Tidying stat outputs
          "scatterplot3d", # 3D Scatterplot
          "rgl",           # Interactive 3D Scatterplot
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
# Example for multiple regression
?mtcars
cars <- as_tibble(mtcars, rownames = 'name') %>% 
  select(name, cyl, vs, am, mpg, disp, hp, wt) %>%
  mutate(cyl       = as_factor(cyl), # Convert numbers into factors
         vs        = as_factor(vs),
         am        = as_factor(am), 
         disp_sqrt = sqrt(disp)) %>% # Needed transformation 
  print() # Possibly can use drat, wt, gear, and carb

# Cylinders, engine, and transmission will be grouping variables
cars %>% group_by(cyl, vs, am) %>% summarise(count = n())
# cyl has 3 levels while vs and am has 2 levels each.

# Identify a colorblind friendly palette to label your categorical variable
RColorBrewer::display.brewer.all(colorblindFriendly = TRUE)
RColorBrewer::display.brewer.pal(3, "Dark2") # My choice
# Save it as an object to call later
mycolor <- brewer.pal(3, "Dark2")

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
pairs(cars[, 5:8], # Variables being analyzed
      pch = 19, # symbol type
      diag.panel = panel.hist, # Insert histogram
      cex.labels = 2, # Font size 
      col = mycolor[cars$am], # Color according to transmission
      cex = 2) # Symbol size

# Click the zoom button for easier vierwing
# Variables are listed in a diagonal with the data shown as a histogram
# Each intersection shows a scatterplot for the associated variables 
# Each scatterplot is repeated twice with the axes switched

## This is helpful when comparing multiple variables, so you don't have to 
## produce an individual plot for each comparison.

## We are using select variables from the mtcars dataset to show different trends
## in the data and how to apply the correct the best-fitting formulas.

# Possible trends:
# Horse Power x Displacement - Linear (proportional changes)
# Weight x Displacement      - Exponential growth (curves upward)
# MPG x Displacement         - Exponential decay (curves downward)

# Example 1: Horsepower x Displacement ------------------------------------

## Research Question: How does horsepower affect the distance traveled, and
## does this relationship vary across different transmission types?

# Check Assumptions -------------------------------------------------------

# Check normality
## Although both variables are continuous and not necessarily affecting each other,
## you want to put the variable most likely to influence the other as the X-variable
cars_HxD_model <- lm(data = cars, disp ~ hp * am)
cars_HxD_resid <- resid(cars_HxD_model)
shapiro.test(cars_HxD_resid) # Normal Distribution

# Check variance
# I'll use transmission type as my grouping, but you can use any of the 3.
bartlett.test(data = cars, disp ~ am) # Equal variances

# Check residual plot
plot(cars$hp, cars_HxD_resid) %>% # Plot residuals along your x-variable
  abline(h = 0) # Flot a horizontal line at 0 for reference
# Are the residuals equally distributed around the horizontal line?
# I would argue yes, so the (linear) model has the best fit line

# Multiple Regression -----------------------------------------------------

## The syntax is similar to a 2-way ANOVA where you connect your 
## 2 x-variables with "*"

# Use aov() to test hypotheses
cars_HxD_aov <- aov(data = cars, disp ~ hp * am)
tidy(cars_HxD_aov)
# Use lm() to run multiple regression
tidy(cars_HxD_model)

# Interpretting the Results -----------------------------------------------

# Graphing should always be paired with your results
cars %>% 
  ggplot(aes(x = hp, y = disp, color = am, fill = am)) +
  geom_point() +
  geom_smooth(size = 1.5, method = "lm", formula = y ~ x) +
  scale_fill_manual(values = mycolor) +
  scale_color_manual(values = mycolor)

# Whole model results
tidy(cars_HxD_aov)
# Each comparison has a significant effect on displacement
# horsepower:   (df 1, 1;  F = 105;  p < 0.001)
# transmission: (df 1, 1;  F = 28.3; p < 0.001)
# Interaction:  (df 1, 28; F = 6.33; p = 0.0179)

# Multiple regression
tidy(cars_HxD_model)
# The levels of your categorical variable are organized by alphabetical order
# 0 (automatic) - y-intercept: 17.1;        slope = 1.71
# 1 (manual) -    y-intercept: 17.1 + 4.83; slope = 1.71 - 0.747

# If you have > 2 levels, each following level is +/- from the first level
# ex: 17.1 + ___ level; 1.71 + ___ level

# Example 2: Weight x Displacement ----------------------------------------

## Research Questions: How does weight affect the distance a car travels, and
## does the number of cylinders affect this relationship?

# Check Assumptions -------------------------------------------------------

# Check normality
cars_WxD_model <- lm(data = cars, disp ~ wt * cyl)
cars_WxD_resid <- resid(cars_WxD_model)
shapiro.test(cars_WxD_resid) # Normal Distribution

# Check variance
# I'll use transmission type as my grouping, but you can use any of the 3.
bartlett.test(data = cars, disp ~ cyl) # UN-equal variances
# Apply data transformation
bartlett.test(data = cars, disp_sqrt ~ cyl) # Equal variances
# Recheck normality
cars_WxD_model2 <- lm(data = cars, disp_sqrt ~ wt * cyl)
cars_WxD_resid2 <- resid(cars_WxD_model2)
shapiro.test(cars_WxD_resid2) # Normal Distribution

# Check residual plot
plot(cars$wt, cars_WxD_resid2) %>% abline(h = 0) 
# Are the residuals equally distributed around the horizontal line?
# I would argue yes, so the square root model has the best fit line

# Run Multiple Regression -------------------------------------------------

# Use aov() to test hypotheses
cars_WxD_aov <- aov(data = cars, disp_sqrt ~ wt * cyl)
tidy(cars_WxD_aov)
# Use lm() to run multiple regression
tidy(cars_WxD_model2)

# Interpretting the Results -----------------------------------------------

cars %>% 
  ggplot(aes(x = wt, y = disp_sqrt, color = cyl, fill = cyl)) +
  geom_point() +
  geom_smooth(size = 1.5, method = "lm", formula = y ~ x) +
  scale_fill_manual(values = mycolor) +
  scale_color_manual(values = mycolor)

# Whole model results
tidy(cars_WxD_aov)
## Weight and number of cylinders have a sig. effect on distance traveled; however,
## the interaction effect is not significant. 
## Weight:      (df = 1, 2; F = 323;     p < 0.001)
## cylinders:   (df = 2, 2; F = 29.4;    p < 0.001)
## Interaction: (df = 2, 26; F = 0.0634; p = 0.939)

# Multiple regression results
tidy(cars_WxD_model2)
# 4 cyl - y-intercept: 5.73;        (curvilinear) slope: 6.14
# 6 cyl - y-intercept: 5.73 + 1.95; (curvilinear) slope: 6.14 + 0.0773
# 8 cyl - y-intercept: 5.73 + 1.43; (curvilinear) slope: 6.14 - 0.234

# Partial F-Test ----------------------------------------------------------

# Checking Possible Patterns
## I can't ignore that the data looks like it has a exponential growth curve
## when the data is viewed without cylinder groups

# Graph of different models:
cars %>% 
  ggplot(aes(x = wt, y = disp_sqrt)) +
  geom_point() +
  geom_smooth(size = 1.5, method = "lm", formula = y ~ x) + # Straight
  geom_smooth(size = 1.5, method = "lm", color = "red",
              formula = y ~ x + I(x ^ 2)) # Curved

## The fitted lines aren't very different, so they make me second guess 
## myself, but I still want to check it out

# Check assumptions
cars_wt_model <- lm(data = cars, disp_sqrt ~ wt)
cars_wt_resid <- resid(cars_wt_model)
shapiro.test(cars_wt_resid) # Normal Distribution (barely)
# Make arbitrary groups for checking variances
cars %>% summarise(count = n(),
                   grouping = count/3) # Count for 3 equal groups
cars <- cars %>% arrange(wt) %>% # Organize data by x-variable
  mutate(group_wt = rep(c("A", "B", "C"), # Repeat group names
                       c(10, 10, 12))) %>% # Arbitrary groups
  print()
bartlett.test(data = cars, disp_sqrt ~ group_wt) # Equal variances
plot(cars$wt, cars_wt_resid) %>% abline(h = 0) # Not exactly evenly distributed

# Curved model
cars_wt_model2 <- lm(data = cars, disp_sqrt ~ wt + I(wt ^ 2))
cars_wt_resid2 <- resid(cars_wt_model2)
shapiro.test(cars_wt_resid2) # Normal Distribution (larger value)
plot(cars$wt, cars_wt_resid2) %>% abline(h = 0) # Shifted line up; is it okay?

# Compare the models using a partial F-test
anova(cars_wt_model2, cars_wt_model) # p > 0.05, so there is no difference

# Comparing 3 Continuous Variables ----------------------------------------

## These previous questions focused around 2 X-variables (continous & categorical),
## but we also need to consider having 2 continous X-variables.

## Research question: How does weight and horsepower separately affect distance
## traveled, and does a car's weight affect the distance traveled across different
## magnitudes of horsepower?

## I'm not 100% confident how to check assumptions, so I'll check in with my 
## professor and return to this section. In the meantime, let's assume the
## original dataset has a normal distribution and equal variances.

# Whole model - with interaction
cars_WHD_aov <- aov(data = cars, disp ~ wt * hp) 
tidy(cars_WHD_aov)
## Because the interaction effect is not sig, we can remove it to increase
## the robustness of the individual comparisons
cars_WHD_aov <- aov(data = cars, disp ~ wt + hp) # Use a "+" instead of "*"
tidy(cars_WHD_aov)
# Multiple regression - without interaction
cars_WHD_model <- lm(data = cars, disp ~ wt + hp) 
tidy(cars_WHD_model)

# 3D Scatterplot ----------------------------------------------------------

# Shorten the dataset for easier usage
cars_short <- cars %>% select(wt, hp, disp, cyl) %>% print()
# 1st package creates a picture
scatterplot3d::scatterplot3d(cars_short[, 1:3],
              pch = 16, # Point type
              color = brewer.pal(3, "Dark2")[unclass(cars$cyl)],
              angle = 60, # Change viewing angle
              type = "h", # Linetype to show point heights
              main = "3D Scatterplot")$plane3d(cars_WHD_model) # lm() model

# 2nd package creates a 3D interactive model
plot3d(cars$wt,  # x variable
       cars$hp,   # y variable
       cars$disp,  # z variable
       xlab = "Weight",
       ylab = "Horsepower",
       zlab = "Distance",
       col = brewer.pal(3, "Dark2")[unclass(cars$cyl)],
       size = 8)
