library(tidyverse)
library(ggplot2)
library(ggthemes)
library(purrr)

# making a basic function 

fahr_to_cels <- function(temp_f){
    temp_c <- (temp_f - 32) * 5 / 9
    return(temp_c)
}

cels_to_kelv <- function(temp_c){
    temp_k <- temp_c + 273.15
    return(temp_k)
}

# testing code

fahr_to_cels(temp_f = 32)

fahr_to_cels(temp_f = 212)

fahr_to_cels(temp_f = 55)

fahr_to_cels(212)

# making a little more complex function

fahr_to_kelvin <- function(temp_f){
    temp_c <- fahr_to_cels(temp_f)
    temp_k <- cels_to_kelv(temp_c)
    return(temp_k)
}

# testing code

fahr_to_kelvin(32)

# making function for repeated graphing

## read in data

alcohol <- read_table2("custom_function_data/wine_data.txt") # this is a new data file we haven't discussed

## making the function

make_scatter <- function(df, xvar, x_title, header){
    graph <- ggplot(data = df) +
        geom_point(aes_string(x = xvar,
                              y = "cirrhosis_death")) +
        labs(x = x_title,
             y = "Cirrhosis Death Rate",
             title = header) +
        theme_economist()
    return(graph)
}

## testing graph function

make_scatter(alcohol,
             "urban_pop",
             "Percent of Population Living in Urban Area",
             "How State Cirrhosis Death Rate Differs By State Urbanicity")

make_scatter(alcohol,
             "wine_consumption",
             "Wine Consumption Per Capita",
             "How State Cirrhosis Death Rate Differs By In-State Wine Consumption")

# Example of mapping function (load in purrr)

temp_CA_f <- 68
temp_MI_f <- 33
temp_GA_f <- 50
temp_MN_f <- 24

temp_CA_c <- fahr_to_cels(temp_CA_f)
temp_MI_c <- fahr_to_cels(temp_MI_f)
temp_GA_c <- fahr_to_cels(temp_GA_f)
temp_MN_c <- fahr_to_cels(temp_MN_f)

## Step 1 make list

temps_f <- c(temp_CA_f, temp_MI_f, temp_GA_f, temp_MN_f)

## Step 2 use map

temp_c_list <- map(temps_f, function(x) fahr_to_cels(temp_f = x))

### Step 3 make sure it worked

temp_c_list[[1]] # writing the list followed by the position of the object within two brackets allows R to print it

temp_CA_c

temp_c_list[[3]]

temp_GA_c

# functions with conditional code

is_pos_or_neg <- function(num){
    if(num < 0){
        return("Negative")
    }
    if(num >= 0){
        return("Positive")
    }
}

## testing the function out

is_pos_or_neg(5)
is_pos_or_neg(-2)
is_pos_or_neg(0)