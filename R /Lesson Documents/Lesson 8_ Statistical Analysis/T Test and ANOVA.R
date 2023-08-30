library(tidyverse)
library(ggplot2)
library(broom)
library(knitr)
library(gtsummary)

# Reading in Data 

bm <- read_csv("data_to_summarize/body_measures.csv")

# Making New Variable agecat for Anova 

bm <- bm %>%
    mutate(age_cat = case_when(age <= 25 ~ "25 and Younger",
                               age > 25 & age <= 45 ~ "26 to 45",
                               age > 45 ~ "Older than 45"))

# Running a T-Test

## Unequal Variance t.test

bicep_test <- t.test(bicep_girth_flexed ~ gender, data = bm) # The default setting is that there is unequal variance (ie spread)
# the continuous variable between the groups. If there is equal variance you would include the "var.equal = TRUE" argument.

bicep_test # because statistically significant, there is a difference in bicep girth between sexes. 

bicep_test$parameter # this would print out just the degrees of freedom

## Equal Variance t.test

bicep_test_equal <- t.test(bicep_girth_flexed ~ gender, data = bm, var.equal = TRUE)

# Running an ANOVA 

height_anova <- aov(height ~ age_cat, data = bm)

height_anova

summary(height_anova)