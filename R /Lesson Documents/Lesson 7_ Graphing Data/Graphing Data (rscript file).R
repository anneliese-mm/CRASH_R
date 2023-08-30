library(tidyverse)
library(ggplot2)
library(ggthemes)
library(patchwork)

# Reading in Data 

bm <- read_csv("data_to_summarize/body_measures.csv")

# Making New Variable agecat

bm <- bm %>%
    mutate(age_cat = case_when(age <= 25 ~ "25 and Younger",
                               age > 25 & age <= 45 ~ "26 to 45",
                               age >45 ~ "Older than 45"))

# Graphing with ggplot

## histogram examples

ggplot(data = bm) +
    geom_histogram(aes(x = ankle_diameter))

ggplot(data = bm) +
    geom_histogram(aes(x = ankle_diameter), binwidth = 3)

ggplot(data = bm) +
    geom_histogram(aes(x = ankle_diameter), bins = 50)

## scatterplots examples

ggplot(data = bm) +
    geom_point(aes(x = chest_girth, y = thigh_girth, color = gender)) #mapping a color onto a variable, in this case gender

ggplot(data = bm) +
    geom_point(aes(x = chest_girth, y = thigh_girth), color = "green") #setting a color onto all point, in this case green

ggplot(data = bm) +
    geom_point(aes(x = chest_girth, y = thigh_girth, color = gender)) +
    theme_stata() +
    scale_color_manual(values = c("Female" = "darkgreen", "Male" = "turquoise")) +
    scale_x_continuous(limits = c(80, 110),
                       breaks = seq(from = 80, to = 110, by = 5)) +
    labs(color = "Gender", x = "Chest Girth", y = "Thigh Girth", title = "Relationship Between Chest and Thigh Girth") +
    theme(legend.position = "bottom") 

### scatterplot grid examples
    
ggplot(data = bm) +
    geom_point(aes(x = chest_girth, y = thigh_girth)) +
    facet_grid(gender ~ age_cat)

scatter_example <- ggplot(data = bm) +
    geom_point(aes(x = chest_girth,
                   y = thigh_girth)) 

bar_example <- ggplot(data = bm) +
    geom_col(aes(x = gender,
                 y = height)) 

scatter_example + bar_example #scatter will appear on the left

scatter_example / bar_example #scatter will appear on the top