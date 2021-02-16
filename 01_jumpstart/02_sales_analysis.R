# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# JUMPSTART: First Sales Analysis ----

# 1.0 Load libraries ----

# Work horse packages
library(tidyverse)
library(lubridate)

# theme_tq()
library(tidyquant)

# Excel Files
library(readxl)
library(writexl)




# 2.0 Importing Files ----

?read_excel()

bikes_tbl <- read_excel(path = "00_data/bike_sales/data_raw/bikes.xlsx")

bikeshops_tbl <- read_excel("00_data/bike_sales/data_raw/bikeshops.xlsx")

orderlines_tbl <- read_excel("00_data/bike_sales/data_raw/orderlines.xlsx")



# 3.0 Examining Data ----

bikes_tbl

glimpse(bikes_tbl)

bikeshops_tbl

orderlines_tbl

# 4.0 Joining Data ----

?left_join

orderlines_tbl

bikes_tbl

left_join(orderlines_tbl, bikes_tbl, by = c("product.id" = "bike.id"))

bike_orderlines_joined_tbl <- orderlines_tbl %>%
    left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
    left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))

bike_orderlines_joined_tbl

bike_orderlines_joined_tbl %>% glimpse()

# 5.0 Wrangling Data ----




# 6.0 Business Insights ----


# 6.1 Sales by Year ----

# Step 1 - Manipulate




# Step 2 - Visualize



# 6.2 Sales by Year and Category 2 ----


# Step 1 - Manipulate




# Step 2 - Visualize




# 7.0 Writing Files ----


# 7.1 Excel ----


# 7.2 CSV ----


# 7.3 RDS ----