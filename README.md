# BioStatistics
I've recently taken a Biostatistics course, and I learned how to conduct and interpret a handful of statistical analyses. I want to share how to run some of these analyses in R. I won't be able to transcribe all of my notes into GitHub, but I plan on boiling down the key concepts necessary for running specific tests. For the most part, these are general notes for me to revisit if I need to run statistics in R, which ideally will get me moving in the right direction. I hope they can serve the same purpose for you. If you need more in-depth training to run statistics in R, you will have to consult an expert.  

###### Datasets
I am using the datasets provided by Base R. Realistically, you can use most of these datasets for the example code. Using the appropriate dataset depends on your experimental questions, variable types, and statistical tests. You can view the available datasets and their descriptions using:
```
library(help = "datasets") # General info about all datasets
??<specific dataset> # Pulls up help screen for the specific dataset
 ```
Some good candidates for statistical analyses that I am considering to include: *CO2*, *PlantGrowth*, *Titanic*, *UCBAdmissions*, *iris*, *USJudgeRatings*, *chickwts*, *faithful*, *swiss*, *warpbreaks*, and *mtcars*. 

###### Assumptions
The statistical tests use Base R functions; however, I like to use tidyverse packages, so my R code will be dominated by their functions. I am assuming that my audience has some general knowledge about tidyverse packages. I provide explanations for most of my functions at least once, though. The following are some explanations of packages that I use:
  *dyplyr* for dataset manipulations, *magrittr* for organizing manipulations ("%>%" are called pipes), *ggplot2* for graphing, and *broom* for converting Base R outputs into tibbles. Also, I use non-tidyverse packages including "*ggpubr* for combining multiple graphs into 1 plot and *kableExtra* for converting tibbles into tables ready for export. 

### Statistical Tests
I address 3 major parts of statistical analyses: checking assumptions, conducting tests, and exporting graphs & tables. Statistical tests include: 
- T-tests
- 1- and 2-way ANOVAs
- Linear regressions
- Chi-squared analyses
- Logistic regressions
