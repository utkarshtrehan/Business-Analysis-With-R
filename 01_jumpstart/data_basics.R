# DATA BASICS -----

library(tidyverse)


# Data Types ----

?c

a <- c(1, 2, 3)
a

a %>% class()

b <- c("low", "medium", "high")
b

b %>% class()

# Data Structure ----

ab_tbl <- tibble(
    a,
    b
)
ab_tbl


read_rds("00_data/bike_sales/data_wrangled/bike_orderlines.rds")
