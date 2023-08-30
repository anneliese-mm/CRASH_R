library(tidyverse)
library(gtsummary)

# Reading in Data

bm_raw <- read_csv("data_to_summarize/body_measures.csv")

# Creating some categorical variables
bm_bmi <- bm_raw %>%
    mutate(bmi = round(weight/(height/100)^2,1)) %>%
    mutate(over_weight = if_else(bmi >= 25, 1, 0),
           obese = if_else(bmi >= 30, 1, 0),
           age_cat = if_else(age > 30, "Over 30", "30 or Under"))

# Create a descriptive table with tbl_summary()
bm_bmi %>%
    select(obese, over_weight, age_cat, gender, shoulder_girth,
           wrist_diameter, ankle_girth) %>%
    tbl_summary()

# Change the statistic presented for continuous variables to report mean(sd)
bm_bmi %>%
    select(obese, over_weight, age_cat, gender, shoulder_girth,
           wrist_diameter, ankle_girth) %>%
    tbl_summary(statistic = all_continuous() ~ "{mean} ({sd})")

# Change the statistic presented for categorical variables to just report %
bm_bmi %>%
    select(obese, over_weight, age_cat, gender, shoulder_girth,
           wrist_diameter, ankle_girth) %>%
    tbl_summary(statistic = list(all_continuous() ~ "{mean} ({sd})",
                                 all_categorical() ~ "{p}%"))

# Add labels
bm_bmi %>%
    select(obese, over_weight, age_cat, gender, shoulder_girth,
           wrist_diameter, ankle_girth) %>%
    tbl_summary(statistic = list(all_continuous() ~ "{mean} ({sd})",
                                 all_categorical() ~ "{p}%"),
                label = list(obese ~ "Obese",
                             over_weight ~ "Overweight/Obese",
                             age_cat ~ "Age Category",
                             gender ~ "Gender",
                             shoulder_girth ~ "Shoulder Girth",
                             wrist_diameter ~ "Wrist Diameter",
                             ankle_girth ~ "Ankle Girth"))

# Split sample by a categorical variable, in this case age category
bm_bmi %>%
    select(obese, over_weight, age_cat, gender, shoulder_girth,
           wrist_diameter, ankle_girth) %>%
    tbl_summary(by = age_cat, # add by = 
                statistic = list(all_continuous() ~ "{mean} ({sd})",
                                 all_categorical() ~ "{p}%"),
                label = list(obese ~ "Obese",
                             over_weight ~ "Overweight/Obese",
                             age_cat ~ "Age Category",
                             gender ~ "Gender",
                             shoulder_girth ~ "Shoulder Girth",
                             wrist_diameter ~ "Wrist Diameter",
                             ankle_girth ~ "Ankle Girth"))

# Add overall column by adding add_overall()
bm_bmi %>%
    select(obese, over_weight, age_cat, gender, shoulder_girth,
           wrist_diameter, ankle_girth) %>%
    tbl_summary(by = age_cat,
                statistic = list(all_continuous() ~ "{mean} ({sd})",
                                 all_categorical() ~ "{p}%"),
                label = list(obese ~ "Obese",
                             over_weight ~ "Overweight/Obese",
                             #age_cat ~ "Age Category", # when you add_overall, remove this label
                             gender ~ "Gender",
                             shoulder_girth ~ "Shoulder Girth",
                             wrist_diameter ~ "Wrist Diameter",
                             ankle_girth ~ "Ankle Girth")) %>%
    add_overall()

# Test differences between groups with add_p()

bm_bmi %>%
    select(obese, over_weight, age_cat, gender, shoulder_girth,
           wrist_diameter, ankle_girth) %>%
    tbl_summary(by = age_cat,
                statistic = list(all_continuous() ~ "{mean} ({sd})",
                                 all_categorical() ~ "{p}%"),
                label = list(obese ~ "Obese",
                             over_weight ~ "Overweight/Obese",
                             #age_cat ~ "Age Category", # when you add_overall, remove this label
                             gender ~ "Gender",
                             shoulder_girth ~ "Shoulder Girth",
                             wrist_diameter ~ "Wrist Diameter",
                             ankle_girth ~ "Ankle Girth")) %>%
    add_overall() %>%
    add_p()

# Change to a t-test for continuous variables

bm_bmi %>%
    select(obese, over_weight, age_cat, gender, shoulder_girth,
           wrist_diameter, ankle_girth) %>%
    tbl_summary(by = age_cat,
                statistic = list(all_continuous() ~ "{mean} ({sd})",
                                 all_categorical() ~ "{p}%"),
                label = list(obese ~ "Obese",
                             over_weight ~ "Overweight/Obese",
                             #age_cat ~ "Age Category", # when you add_overall, remove this label
                             gender ~ "Gender",
                             shoulder_girth ~ "Shoulder Girth",
                             wrist_diameter ~ "Wrist Diameter",
                             ankle_girth ~ "Ankle Girth")) %>%
    add_overall() %>%
    add_p(test = all_continuous() ~ "t.test")