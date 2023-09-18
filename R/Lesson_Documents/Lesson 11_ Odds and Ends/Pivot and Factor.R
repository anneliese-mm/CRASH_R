library(tidyverse)


## Wide and Long Data


## Example of a wide data set

blood_pressure_data <- read_csv("pivot_data/bp_data.csv")

# Move from wide to long data:
blood_pressure_long1 <- blood_pressure_data %>%
    pivot_longer(cols = starts_with("bp_week"),
                 names_to = "week")

# You can add the values_to argument to name the new column:
blood_pressure_long2 <- blood_pressure_data %>%
    pivot_longer(cols = starts_with("bp_week"),
                 names_to = "week",
                 values_to = "blood_pressure")

# using custom function to clean up week values

blood_pressure_long3 <- blood_pressure_data %>%
    pivot_longer(cols = starts_with("bp_week"),
                 names_to = "week",
                 values_to = "blood_pressure",
                 names_transform = list(week = function(x) str_remove(x, "bp_week")))

# making spaghetti plot from long data

ggplot(data = blood_pressure_long3) +
    geom_line(aes(x = week,
                  y = blood_pressure,
                  group = patient_id),
              alpha = 0.4) +
    labs(x = "Time (Weeks)",
         y = "Blood Pressure (mmHg)",
         title = "Change in Blood Pressure over Time") +
    theme_bw()
 
# pivoting long to wide 

## example of long data

gapminder_data <- read_csv("pivot_data/reduced_gapminder.csv")

gapminder_wide1 <- gapminder_data %>%
    pivot_wider(names_from = year, 
                values_from = lifeExp)

# Adding the name prefixes saves a renaming step and is a good practice:

gapminder_wide2 <- gapminder_data %>%
    pivot_wider(names_from = year,
                values_from = lifeExp,
                names_prefix = "life_exp_y")

##########################################################################

# Factor Variables

bm <- read_csv("data_to_summarize/body_measures.csv")

factor_bm <- bm %>%
    mutate(gender_fac = factor(gender, levels = c("Male", "Female")))