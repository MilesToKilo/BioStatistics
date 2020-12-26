# Outline -----------------------------------------------------------------

# 1. Assumptions (transformations)
# 2. 2-Way ANOVA with interactions and Tukey's Test
# 3. Convert output into table
# 4. Graph
# 5. Reporting results

# Load Data ---------------------------------------------------------------

# Load packages
pack <- c("tidyverse",    # Main processing package
          "ggpubr",       # Grouping plots
          "broom",        # Tidying stat outputs
          "kableExtra")   # Export tables

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

## Quick note: 2-way ANOVAs with interactions investigate whether 2 X-variables
## significantly influence a Y-variable, and also checks if different levels 
## across the 1st X-variable influences the relationship of the 2nd X-variable

## For that reason, each dataset needs at least 2 categorical X-variables and
## 1 continuous Y-variable. 

# View data
as_tibble(warpbreaks) %>% group_by(wool, tension) %>% # 2 x-variables
  summarise(.groups = "keep", count = n()) # Sample size

as_tibble(mtcars) %>% # vs (engine): 0 = V-shaped, 1 = straight
  group_by(vs, am) %>% # am: Auto = 0, Manual = 1
  summarise(.groups = "keep", count = n()) # Unbalanced design

## Although the sample size is not balanced across all groups, that's why we
## check our assumptions to verify it's acceptable to compare samples.

# Load data
warp <- as_tibble(warpbreaks) %>% select(wool, tension, breaks) %>% 
  mutate(breaks_sq = sqrt(breaks)) %>% # Create another column variable
  print() # Transformation is needed
vroom <- as_tibble(mtcars) %>% select(vs, am, cyl, mpg, disp) %>% 
  mutate(vs  = as_factor(vs), # Read as numbers, but I need them as factors
         am  = as_factor(am), 
         cyl = as_factor(cyl)) %>% 
  print() # Including cyl for an X-variable with 3 levels; optional comparison

# Example 1: Warpbreaks ---------------------------------------------------

## 2-Way ANOVAs with interactions actually have 3 research questions: 1 for each 
## X-Variable and another for their interaction. 

## Research Questions:
## 1. Does wool type affect the number of breaks?
## 2. Does the amount of tension affect the number of breaks?
## 3. Does the number of breaks caused by wool type vary across different levels
## of tensions?

# Check Assumptions -------------------------------------------------------

## The steps to checking assumptions were described in another file, so
## I'll provide the code for the objective tests with minimal explanation here:

## 2-way ANOVAs have an additional level of complexity, so their models need
## adjustments. aov(y ~ x * X) OR aov(y ~ interaction(x, x)). For some reason,
## Bartlett tests require interaction(x, x) syntax.

shapiro.test(resid(aov(breaks ~ wool * tension, data = warp))) # Normal
bartlett.test(breaks ~ interaction(wool, tension), data = warp) # Un-equal var
# Check again with transformation
shapiro.test(resid(aov(breaks_sq ~ wool * tension, data = warp))) # Normal
bartlett.test(breaks_sq ~ interaction(wool, tension), data = warp) # Equal var
# Remember that you must report results using the transformed data, not the OG

# 2-Way ANOVA and Tukey's Test --------------------------------------------

warp_model <- aov(breaks_sq ~ wool * tension, data = warp)
summary(warp_model) 
# convert into a tibble
warp_aov <- tidy(warp_model) %>% select(term, df, statistic, p.value) %>% 
  print()
## Results show a significant influence by tension and the interaction between
## wool and tension. Wool is not significant but is borderline!

# Conduct pairwise comparisons
TukeyHSD(warp_model) # That's A LOT of comparisons
# convert into a tibble
warp_tukey <- tidy(TukeyHSD(warp_model)) %>% # arrange(adj.p.value) %>% 
  select(term, contrast, adj.p.value) %>% 
  print() 
## This isn't always preferable because I like having the single comparisons
## listed at the top, but it does help looking at what's significant

# Creating Tables ---------------------------------------------------------

# Convert a tibble into a table using kableExtra::kbl()
warp_tableA <- warp_aov %>% 
  kbl(caption = "Table 1. 2-Way ANOVA: Breaks ~ Wool * Tension") %>% 
  kable_classic(html_font = "Courier New") %>% # Font
  kable_styling(bootstrap_options = c("striped", # Shades every other row
                                      "condensed"), # Decreases empty space
                full_width = FALSE) %>% # Changes table width
  row_spec(0, bold = TRUE) %>% # Bold column names
  footnote( # Add footnotes describing table for readers
    general = "adjusted p-values < 0.05 are significantly different.") %>% 
  print()

warp_tableT <- warp_tukey %>% 
  kbl(caption = "Table 2. Tukey's Test: Breaks ~ Wool * Tension") %>% 
  kable_classic(html_font = "Courier New") %>% # Font
  kable_styling(bootstrap_options = c("striped", # Shades every other row
                                      "condensed"), # Decreases empty space
                full_width = FALSE) %>% # Changes table width
  row_spec(0, bold = TRUE) %>% # Bold column names
  footnote( # Add footnotes describing table for readers
    general = "adjusted p-values < 0.05 are significantly different.") %>% 
  print()

# Graphing ----------------------------------------------------------------

## I think the best way to visualize a 2-Way ANOVA w/interactions is using an
## interaction plot. It shows the means and standard errors of the 1st X-variable
## and connects the grouped points of the 2nd X-variable with a line. 

# Calculate summary statistics
warp_sum <- warp %>% group_by(wool, tension) %>% 
  summarise(breaks_mean    = mean(breaks_sq),
            breaks_error   = sd(breaks_sq) / sqrt(n())) %>% 
  print()

## Be mindful of which X-variable will be on the x-axis and which will be used
## as a grouping. Revisit your research questions and make sure your graphs will
## accurately display the trends you're interested in.

# I want to see the change in breaks for a wool type with increasing tension. 
warp_plot <- warp_sum %>% # Comment plot object to show graph
  ggplot(aes(x = tension, y = breaks_mean, # Amount of tension on X-axis
             group = wool, # Grouped by wool type
             color = wool)) + # Color according to group
  geom_line(lwd = 2, position = position_dodge(width = 0.5)) + # Offset lines
  geom_point(position = position_dodge(width = 0.5), size = 4) + # Offset points
  geom_errorbar(position = position_dodge(width = 0.5), aes( # Match each dodge
    ymin = breaks_mean - breaks_error, 
    ymax = breaks_mean + breaks_error), 
    width = 0.25, lwd = 1.25)

# How to Report Results ---------------------------------------------------

# Recall we produced 2 tables and 1 graph for your results
warp_tableA # 2-Way ANOVA 
warp_tableT # Tukey's Test
warp_plot   # Interaction plot

# How do you report values from all this information??

# The 2-Way ANOVA addresses your research questions.
## Wool types do not significantly affect the number of breaks 
## (df = 1, 2; F = 3.0222; p = 0.0885).
## The amount of tension significantly affects the number of breaks
## (df = 2, 2; F = 8.2752; p = 0.0008).
## We have evidence of a significant interaction effect between wool type and 
## amount of tension on the number of breaks (df = 2, 48; F = 3.7500; p = 0.0307).

## The Tukey's Test addresses pairwise comparisons, and you must choose which
## comparisons are the most important for your story/theme of your research.
## I would pick comparisons that are sig. and not sig. different to build a 
## well-rounded story. What was most interesting to you, or maybe shocking?
## Although you won't report all of the differences, you can always provide
## a supplementary table that includes all comparisons for your readers. 

# Interpretting your interaction plot
## Remember that you're comparing 3 different groups: Wool type, tension, and
## the interactions. For wool type, visually average the heights of all points
## from each type and compare the heights. For tension, visually average the
## heights of the each tension group and compare the heights. Lastly for the 
## interaction, compare the slopes and if they aren't parallel, they are likely
## sig. different. These visual cues will help you interpret your data, and then
## share that with your audience. 

# Reporting your interaction plot
## Your plot should compliment the results you report by referencing both the
## table and the plot. I would recommend describing how much the means varied
## between groups. You should compare the means by percentage inc-/decrease
## or by X-fold differences. These comparisons help your readers easily grasp 
## your data. 

# Example 2: mtcars -------------------------------------------------------

vroom # You might want to consider changing the values into words

# Research questions:
## 1. Does engine type affect car performance; specifically, mpg?
## 2. Is car mileage affected by transmission type?
## 3. Does the influence of engine type on car mileage vary depending on 
## its transmission?

# Check assumptions
shapiro.test(resid(aov(mpg ~ vs * am, data = vroom))) # Normal
bartlett.test(mpg ~ interaction(vs, am), data = vroom) # Equal var

# 2-Way ANOVA
vroom_model <- aov(mpg ~ vs * am, data = vroom)
vroom_aov <- tidy(vroom_model) %>% select(term, df, statistic, p.value) %>% 
  print()
## Results show a sig. effect for each X-variable, but no sig. interaction effect

# Pairwise comparisons
vroom_tukey <- tidy(TukeyHSD(vroom_model)) %>% 
  select(term, contrast, adj.p.value) %>% 
  print() # This won't run if your variables are not factors

# Convert to tables
vroom_tableA <- vroom_aov %>% 
  kbl(caption = "Table 3. 2-Way ANOVA: MPG ~ Engine * Transmission") %>% 
  kable_classic(html_font = "Courier New") %>% 
  kable_styling(bootstrap_options = c("striped", "condensed"), 
                full_width = FALSE) %>% 
  row_spec(0, bold = TRUE) %>% 
  footnote(general = "adjusted p-values < 0.05 are significantly different.") %>% 
  print()
vroom_tableT <- vroom_tukey %>% 
  kbl(caption = "Table 4. Tukey's Test: MPG ~ Engine * Transmission") %>% 
  kable_classic(html_font = "Courier New") %>% 
  kable_styling(bootstrap_options = c("striped", "condensed"), 
                full_width = FALSE) %>% 
  row_spec(0, bold = TRUE) %>% 
  footnote(general = "adjusted p-values < 0.05 are significantly different.") %>% 
  print()

# Graph interaction plots
vroom_sum <- vroom %>% group_by(vs, am) %>% 
  summarise(mpg_mean    = mean(mpg),
            mpg_error   = sd(mpg) / sqrt(n())) %>% 
  print()

vroom_plot <-
  vroom_sum %>% # Comment plot object to show graph
  ggplot(aes(x = vs, y = mpg_mean, # Amount of tension on X-axis
             group = am, # Grouped by wool type
             color = am)) + # Color according to group
  geom_line(lwd = 2, position = position_dodge(width = 0.5)) + # Offset lines
  geom_point(position = position_dodge(width = 0.5), size = 4) + # Offset points
  geom_errorbar(position = position_dodge(width = 0.5), aes( # Match each dodge
    ymin = mpg_mean - mpg_error, 
    ymax = mpg_mean + mpg_error), 
    width = 0.25, lwd = 1.25)

# Reporting results
vroom_tableA # 2-Way ANOVA 
vroom_tableT # Tukey's Test
vroom_plot   # Interaction plot

# Engine - Sig. effect (df = 1, 1; F = 41.1963; p < 0.001)
# Transmission - Sig. effect (df = 1, 1; F = 22.9021; p < 0.001)
# Interaction - No sig. effect (df = 1, 28; F = 1.3283; p = 0.2589)

# You should try writing this results section. 
# Maybe I'm just lazy and don't want to write it myself...
