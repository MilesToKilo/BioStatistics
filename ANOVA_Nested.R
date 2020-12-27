# Outline -----------------------------------------------------------------

# 1. Assumptions
# 2. Nested ANOVA & Tukey's Test
# 3. Convert output into table
# 4. Graph
# 5. Reporting results

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

## Nested ANOVAs require a catgorical x-variable (Nesting) to have a second layer of 
## factors (Nested) that are group specific. When comparing these groups, you can
## only compare the factors within a group because your statistical model cannot
## account for hidden variables that may be influencing your y-variable.

# Check data
as_tibble(CO2) %>% group_by(Type, Plant) %>% # (nesting, nested)
  summarize(.groups = "keep", count = n()) 
## Although the dataset has 12 different plants, each location has 3 site-specific 
## plants. Hidden factors/variables pertaining to each site may be influencing 
## those plants and their response variables, so models would be statistically 
## inaccurate to compare plants from different sites.  

# Load data
plant <- as_tibble(CO2) %>% 
  select(Type, Plant, Treatment, uptake) %>% 
  print()

# Example 1: CO2 - Plant --------------------------------------------------

## Datasets only provides 1 good dataset for Nested ANOVAs that I'm aware of. 
## Example 1 will use specific plants as the nested variable. 

## Nested ANOVAs have 2 research questions: 1 to compare nestings, and another
## to compare the (nesting specific) nested.

## Research Questions:
## 1. Does location affect CO2 uptake in plants? 
## 2. Do plants vary in CO2 uptake? 

# Since these plants are site-specific, we cannot test for an interaction effect.

# Check Assumptions -------------------------------------------------------

## Like 2-Way ANOVAs, Nested ANOVAs have an additional level of complexity, so 
## their models need adjustments. aov(y ~ Nesting / Nested) OR 
## aov(y ~ interaction(Nesting:Nested)). For some reason, Bartlett tests 
## require interaction(Nesting:Nested) syntax.

shapiro.test(resid(aov(uptake ~ Type / Plant, data = plant))) # Non-normal
bartlett.test(uptake ~ interaction(Type:Plant), data = plant) # Un-equal

## Unfortunately, I have tried a few transformations (Square-root, Log and Log10)
## and none could resolve these assumptions. A major issue is the small sample
## size, so I'm going to pretend like the dataset meets both assumptions. 
## If you would like, you could supply your own dataset or generate one, too, to 
## perform a proper Nested ANOVA, but that's too much work for my free time. 
## I hope you can understand. 

# Nested ANOVA and Tukey's Test -------------------------------------------

# Perform a Nested ANOVA
plant_modelP <- aov(uptake ~ Type / Plant, data = plant)
# Convert into table
plant_tablePA <- tidy(plant_modelP) %>% # 1st convert to tibble
  select(term, df, statistic, p.value) %>% # Select needed variables
  kbl(caption = "Table 1. Nested ANOVA: CO2 Uptake ~ Type / Plant") %>% 
  kable_classic(html_font = "Courier New") %>% # Font
  kable_styling(bootstrap_options = c("striped", # Shades every other row
                                      "condensed"), # Decreases empty space
                full_width = FALSE) %>% # Changes table width
  row_spec(0, bold = TRUE) %>% # Bold column names
  footnote( # Add footnotes describing table for readers
    general = "adjusted p-values < 0.05 are significantly different.") %>% 
  print()

# Perform Tukey's Test
TukeyHSD(plant_modelP) # What?!?
## The code functions similarly to a 2-Way ANOVA, but comparisons between nesting
## groups cannot be done, so that's why it returns NA values. You can fix this
## issue by filtering out NA values.
# Convert to table
plant_tablePT <- tidy(TukeyHSD(plant_modelP)) %>% # 1st convert to tibble
  select(term, contrast, adj.p.value) %>% # Select needed variables
  filter(!is.na(adj.p.value), # 57 reportable comparisons...
         adj.p.value < 0.05) %>% # Only show sig. diff. (11 total)
  kbl(caption = "Table 2. Tukey's Test: CO2 Uptake ~ Type / Plant") %>% 
  kable_classic(html_font = "Courier New") %>% # Font
  kable_styling(bootstrap_options = c("striped", # Shades every other row
                                      "condensed"), # Decreases empty space
                full_width = FALSE) %>% # Changes table width
  row_spec(0, bold = TRUE) %>% # Bold column names
  footnote( # Add footnotes describing table for readers
    general = "Only adjusted p-values < 0.05 are reported.") %>% 
  print()

# Graphing ----------------------------------------------------------------

# I think that Bar plots are good for visualizing nested comparisons

# Calculate summary statistics
plant_sumP <- plant %>% group_by(Type, Plant) %>% 
  summarise(uptake_mean    = mean(uptake),
            uptake_error   = sd(uptake) / sqrt(n()),
            
            .groups = "keep") %>% 
  print()

# Plot (basic) bar plot
plant_plot1 <- # Comment to see plot
  plant_sumP %>% 
  ggplot(aes(x = Plant, y = uptake_mean, fill = Type)) +
  geom_bar(stat = "identity") +
  geom_errorbar(width = 0.1, lwd = 1, aes(
    ymin = uptake_mean - uptake_error, ymax = uptake_mean + uptake_error))

# How to report results ---------------------------------------------------

# Recall we produced 2 tables and 1 graph for your results
plant_tablePA # Nested ANOVA 
plant_tablePT # Tukey's Test
plant_plot1   # Bar plot

## The steps are similar to those explained in the 2-Way ANOVA file. I'll
## list out the statements for you to double check where you need to pull values.

## Location: Location has a sig. effect on plant CO2 uptake
## (df = 1, 10; F = 50.0166; p < 0.001).
## Plant: Plants significantly vary in CO2 uptakes rates 
## (df = 10, 72; F = 2.2243; p = 0.0255).

# Example 2: CO2 - Treatment ----------------------------------------------

## Although treatment types are the same across the 2 different locations, the 
## nested groups are specific to their location, and location could potentially 
## have hidden variables resulting in different CO2 uptake rates. This means
## that 2-Way ANOVAs are NOT accurate models; a nested model is more accurate. 

# Research Questions: 
## 1. Does location affect CO2 uptake in plants? 
## 2. Do treatments cause variations in CO2 uptake? 

# Check assumptions
shapiro.test(resid(aov(uptake ~ Type / Treatment, data = plant))) # Non-normal
bartlett.test(uptake ~ interaction(Type:Treatment), data = plant) # Un-equal
## This and transformations would not meet the assumptions, so we're gonna act
## like the dataset is both normal and has equal variances. 

# Perform Nested ANOVA
plant_modelT <- aov(uptake ~ Type / Treatment, data = plant)
# Convert into table
plant_tableTA <- tidy(plant_modelT) %>% # 1st convert to tibble
  select(term, df, statistic, p.value) %>% # Select needed variables
  kbl(caption = "Table 3. Nested ANOVA: CO2 Uptake ~ Type / Treatment") %>% 
  kable_classic(html_font = "Courier New") %>% # Font
  kable_styling(bootstrap_options = c("striped", # Shades every other row
                                      "condensed"), # Decreases empty space
                full_width = FALSE) %>% # Changes table width
  row_spec(0, bold = TRUE) %>% # Bold column names
  footnote( # Add footnotes describing table for readers
    general = "adjusted p-values < 0.05 are significantly different.") %>% 
  print()

# Perform Tukey's Test
plant_tableTT <- tidy(TukeyHSD(plant_modelT)) %>% # 1st convert to tibble
  select(term, contrast, adj.p.value) %>% # Select needed variables
  filter(!is.na(adj.p.value)) %>% # Filter out NAs
  kbl(caption = "Table 4. Tukey's Test: CO2 Uptake ~ Type / Treatment") %>% 
  kable_classic(html_font = "Courier New") %>% # Font
  kable_styling(bootstrap_options = c("striped", # Shades every other row
                                      "condensed"), # Decreases empty space
                full_width = FALSE) %>% # Changes table width
  row_spec(0, bold = TRUE) %>% # Bold column names
  footnote( # Add footnotes describing table for readers
    general = "Only adjusted p-values < 0.05 are reported.") %>% 
  print()

# Plot (basic) bar plot
plant_sumT <- plant %>% group_by(Type, Treatment) %>% 
  summarise(uptake_mean    = mean(uptake),
            uptake_error   = sd(uptake) / sqrt(n()),
            
            .groups = "keep") %>% 
  print()

plant_plot2 <- # Comment to see plot
  plant_sumT %>% 
  ggplot(aes(x = Treatment, y = uptake_mean, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1)) +
  geom_errorbar(width = 0.1, lwd = 1, position = position_dodge(width = 1), 
                aes(ymin = uptake_mean - uptake_error, 
                    ymax = uptake_mean + uptake_error))

# Reporting results
plant_tableTA # Nested ANOVA 
plant_tableTT # Tukey's Test
plant_plot2   # Bar plot

## Location: Location has a sig. effect on plant CO2 uptake
## (df = 1, 2; F = 52.5086; p < 0.001).
## Plant: Treatment significantly affect plant CO2 uptakes rates 
## (df = 2, 80; F = 9.4691; p = 0.0002).
