library(tidyverse)
library(ggplot2)
library(broom)
library(knitr)
library(gtsummary)

# Reading in Data 

bm <- read_csv("data_to_summarize/body_measures.csv")

# Making New Variable agecat

bm <- bm %>%
    mutate(age_cat = case_when(age <= 25 ~ "25 and Younger",
                               age > 25 & age <= 45 ~ "26 to 45",
                               age > 45 ~ "Older than 45"))

## scatterplots examples

ggplot(data = bm) +
    geom_point(aes(x = chest_girth, y = thigh_girth))

## linear regression model

model_1 <- lm(thigh_girth ~ age, data = bm)

model_1_glance <- glance(model_1)

model_1_glance

model_1_tidy <- tidy(model_1, conf.int = TRUE)

kable(model_1_tidy)

### linear regression with a categorical predictor

model_2 <- lm(thigh_girth ~ age + gender, data = bm)

model_2_tidy <- tidy(model_2, conf.int = TRUE)

tbl_regression(model_2)
