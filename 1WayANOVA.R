# Outline -----------------------------------------------------------------

# 1. Assumptions
# 2. 1-Way ANOVA & Tukey's Test
# 3. Convert output into table
# 4. Graph

# Load Data ---------------------------------------------------------------

# Load packages
pack <- c("tidyverse",    # Main processing package
          "ggpubr",       # Grouping plots
          "broom",        # Tidying stat outputs
          "kableExtra",   # Export tables
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
chick <- as_tibble(chickwts) %>% select(feed, weight) %>% print()
plant <- as_tibble(PlantGrowth) %>% select(group, weight) %>% print()

# Example 1: Chickwts -----------------------------------------------------

# Check Assumptions -------------------------------------------------------

## The steps to checking assumptions were described in another file, so
## I'll provide the code for the objective tests with minimal explanation here:

chick_resid <- resid(aov(weight ~ feed, data = as_data_frame(chick)))
shapiro.test(chick_resid) # p > 0.05, normal dsitribution
bartlett.test(data = chick, weight ~ feed) # p > 0.05, equal var

## I'm only using examples that meet both assumptions necessary for ANOVAs.
## If your assumptions are not met, either apply transformations to your data
## or use a non-parametric test. I will not be covering either in this module.

# 1-Way ANOVA & Tukey's Tests ---------------------------------------------

## ANOVA stands for ANalysis Of VAriances. ANOVAs compare variances between
## groups to produce a F-ratio; think of this as the test-statistic for applying
## the correct statistical analysis. The p-value comes from the comparison between
## population (or sample) means. 

# aov(y ~ x) <- this formula can become more complex depending on the model.

# Research Question: Does type of feed affect chicken growth?
chick_model <- aov(weight ~ feed, data = chick)
summary(chick_model) # Show the results

## Important values that I regularly report are the F-value, Df, and 
## p-value. The F-value and Degrees of Freedom (df) are associated with your 
## F-ratio, and the F-ratio compares the variation caused by your
## x-variable with the variation caused by randomness. The p-value shows 
## if the populations' means are significantly different. 
## Basically, the F-ratio shows you applied the correct test, and
## the p-value shows your research findings. 

summary(chick_model) # p < 0.05, Sig. Diff!
## Because this test compares more than 1 group, the p-value only shows the 
## presence of a significant difference, but does not reveal between which 
## groups. To identify the significantly different groups, run a Tukey's Test
TukeyHSD(chick_model) # Only need p-value
## The adjusted p-value shows that R incorporated the necessary adjustments
## from the ANOVA to Tukey Test. I don't know what it actually does, but 
## this function is trusted and regularly used by the scientific community.

## With this many comparisons, the Tukey's test reveals multiple that are 
## either significantly different or not. In the next section, I'll show
## you how to organize the results.

# Convert Outputs into Exportable Tables ----------------------------------

# Use broom::tidy() to convert models into a tibble
summary(chick_model) # Base R output
chick_aov <- tidy(chick_model) %>% 
  select(term, df, statistic, p.value) %>% 
  print() 

# Sometimes the p-values are different; is that bad?
## The difference is in the threshold for how many digits to report. 
## Base R only reports up to 16 decimal (I think) while broom goes beyond. 
## I don't know what broom's actual cutoff is, though.

chick_tukey <- tidy(TukeyHSD(chick_model)) %>% 
  select(term, contrast, adj.p.value) %>% # Only selecting what I need to report
  arrange(desc(adj.p.value)) %>% # Organize from highest to lowest
  print() 

# Convert a tibble into a table using kableExtra::kbl()
chick_aov %>% 
  kbl(caption = "Table 1. 1-Way ANOVA: Weight ~ Chicken Feed") %>% 
  kable_classic(html_font = "Courier New") %>% # Font
  kable_styling(bootstrap_options = c("striped", # Shades every other row
                                      "condensed"), # Decreases empty space
                full_width = FALSE) %>% # Changes table width
  row_spec(0, bold = TRUE) %>% # Bold column names
  footnote( # Add footnotes describing table for readers
    general = "adjusted p-values < 0.05 are significantly different.") 

chick_tukey %>% 
  kbl(caption = "Table 2. Tukey's Test: Weight ~ Chicken Feed") %>% 
  kable_classic(html_font = "Courier New") %>% # Font
  kable_styling(bootstrap_options = c("striped", # Shades every other row
                                      "condensed"), # Decreases empty space
                full_width = FALSE) %>% # Changes table width
  row_spec(0, bold = TRUE) %>% # Bold column names
  footnote( # Add footnotes describing table for readers
    general = "adjusted p-values < 0.05 are significantly different.") 

## Although the p-value is reported as zeros, you want to report it as 
## "p < 0.001" because it can never be a zero chance of an alternative outcome.
## The same with a p-value of 1; it is actually a decimal really close to 1.

## I have not been successful in exporting this table. For now, I've resorted
## to taking a screenshot.

# Graphing ----------------------------------------------------------------

## Most of the time, you should have a graph paired with your analyses. I'll
## show examples for bar and violin plots

# First, choose a colorblind friendly palette; I use the package RColorBrewer
display.brewer.all(colorblindFriendly = TRUE) # View all palettes
# Divided into 3 groups: Sequential, Divergent, and Qualitative
display.brewer.pal(6, "Dark2") # Must use a minimum of 3 colors

# Graph Bar Plots 
# Bar plots help visualize the averages and standard error of each group.

# First, calculate summary statistics 
chick_sum <- chick %>% group_by(feed) %>% 
  summarise(
    weight_mean  = mean(weight),
    weight_error = sd(weight) / sqrt(n())) %>% 
  print()

# Graph (basic) bar plot
chick_sum %>% ggplot(aes(x = feed, y = weight_mean, fill = feed)) + 
  geom_bar(stat = "identity") + # Bar height = mean value
  geom_errorbar(aes(ymin = weight_mean + weight_error, # Top error bar
                    ymax = weight_mean - weight_error), # Bottom error bar
                width = 0.35, lwd = 1) + # Bolding error bar
  scale_fill_brewer(palette = "Dark2")

## Although averages and standard errors are important, the graph does not 
## illustrate how the dataset distributions differ. I think knowing the
## distribution would be more revealing or helpful when comparing groups

# Graph Violin Plot 

## Violin plots are more descriptive about the distribution of values although
## it does lack averages and standard error values. I address the shortcoming
## by overlapping the mean as a point.

# Violin plots combine a boxplot and normal distribution
browseURL("https://towardsdatascience.com/understanding-boxplots-5e2df7bcbd51")

# Graph (basic) violin plot
chick %>% 
  ggplot(aes(x = feed, y = weight, fill = feed)) +
  geom_violin(size = 1, color = "black", # Adjusting outline 
              alpha = 0.6) + # Increasing transparency to offset boxplot
  geom_boxplot(width = 0.1, size = 1, color = "black") + # Adjusting outline
  stat_summary(geom = "point", color = "white", # Plot a white point
               fun = "mean") + # Calculate the mean for the point
  scale_fill_brewer(palette = "Dark2")

# Example 2: Plant growth -------------------------------------------------

# Research question: Do plants growth rates vary with different treatments?

# Check assumptions
plant_resid <- resid(aov(weight ~ group, data = as_data_frame(plant)))
shapiro.test(plant_resid) # p > 0.05, normal dsitribution
bartlett.test(data = plant, weight ~ group) # p > 0.05, equal var

# Run 1-way ANOVA and Tukey's Test
plant_model <- aov(weight ~ group, data = plant)
plant_aov <- tidy(plant_model) %>% 
  select(term, df, statistic, p.value) %>% 
  print() # p < 0.05, sig. Diff!
plant_tukey <- tidy(TukeyHSD(plant_model)) %>% 
  select(term, contrast, adj.p.value) %>%
  print() # p < 0.05 only between treatment 1 & 2

# Create tables for reporting results
plant_aov %>% 
  kbl(caption = "Table 1. 1-Way ANOVA: Weight ~ Group") %>% 
  kable_classic(html_font = "Courier New") %>% 
  kable_styling(bootstrap_options = c("striped", "condensed"), 
                full_width = FALSE) %>% 
  row_spec(0, bold = TRUE) %>%
  footnote(general = "adjusted p-values < 0.05 are significantly different.") 

plant_tukey %>% 
  kbl(caption = "Table 2. Tukey's Test: Weight ~ Group") %>% 
  kable_classic(html_font = "Courier New") %>% 
  kable_styling(bootstrap_options = c("striped", "condensed"), 
                full_width = FALSE) %>% 
  row_spec(0, bold = TRUE) %>% 
  footnote(general = "adjusted p-values < 0.05 are significantly different.") 

# Graph (basic) bar plot
plant_sum <- plant %>% group_by(group) %>% 
  summarise(weight_mean  = mean(weight),
            weight_error = sd(weight) / sqrt(n())) %>% print()

plant_sum %>% ggplot(aes(x = group, y = weight_mean, fill = group)) + 
  geom_bar(stat = "identity") + 
  geom_errorbar(aes(ymin = weight_mean + weight_error,
                    ymax = weight_mean - weight_error),
                width = 0.35, lwd = 1) +
  scale_fill_brewer(palette = "Dark2")

# Graph (basic) violin plot
plant %>% 
  ggplot(aes(x = group, y = weight, fill = group)) +
  geom_violin(size = 1, color = "black", alpha = 0.6) + 
  geom_boxplot(width = 0.1, size = 1, color = "black") +
  stat_summary(geom = "point", color = "white", fun = "mean") + 
  scale_fill_brewer(palette = "Dark2")
