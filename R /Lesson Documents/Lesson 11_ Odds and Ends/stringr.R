library(tidyverse)
library(stringr)

# read in data 

work <- read_csv("string_data/work_text.csv")

# get an idea of current levels for work type

work %>%
    group_by(work_type) %>%
    summarize(n = n()) %>%
    view()

# we are trying to make a four leveled categorical variable for work type

work_clean <- work %>%
    mutate(work_cl = case_when(str_detect(work_type, "rivat") ~ "Private Sector", 
                               str_detect(work_type, "reelan") ~ "Self- Employed",
                               str_detect(work_type, "child") ~ "Child-care",
                               str_detect(work_type, "NYS") ~ "Government Sector",
                               TRUE ~ work_type))
                               

work_clean %>%
    group_by(work_cl) %>%
    tally() %>%
    view()

# try again with more possibilities

work_clean <- work %>%
    mutate(work_cl = case_when(str_detect(work_type, "rivat|rivte") ~ "Private Sector",
                               str_detect(work_type, "reelan|trep|free|ploy|aura") ~ "Self-Employed",
                               str_detect(work_type, "child|hild|family|caret") ~ "Child-care",
                               str_detect(work_type, "NYS|bany|HMH|nment|AID|Department") ~ "Government Sector",
                               TRUE ~ work_type)) 

work_clean %>%
    group_by(work_cl) %>%
    tally() %>%
    view()

#getting rid of the unnecessary letters in the front of the ID

work_id_cleaned <- work_clean %>%
    mutate(id_cl = str_extract(id, "[0-9]+") %>%
               as.numeric()) %>%
    select(id_cl, work_cl)
                                          