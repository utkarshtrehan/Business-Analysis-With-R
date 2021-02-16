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

bike_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl %>%
    
    # Separate description into category.1, category.2, and frame.material
    separate(description,
             into = c("category.1", "category.2", "frame.material"),
             sep = " - ",
             remove = TRUE) %>%
    
    # Separate location into city and state
    separate(location,
             into = c("city", "state"),
             sep  = ", ",
             remove = FALSE) %>%
    
    # price extended
    mutate(total.price = price * quantity) %>%
    
    # Reorganize
    select(-X__1, -location) %>%
    select(-ends_with(".id")) %>%
    
    bind_cols(bike_orderlines_joined_tbl %>% select(order.id)) %>%
    
    # Reorder columns
    select(contains("date"), contains("id"), contains("order"),
           quantity, price, total.price,
           everything()) %>%
    
    # Renaming columns
    rename(order_date = order.date) %>%
    set_names(names(.) %>% str_replace_all("\\.", "_")) 

bike_orderlines_wrangled_tbl %>% glimpse()



# 6.0 Business Insights ----


# 6.1 Sales by Year ----

# Step 1 - Manipulate

sales_by_year_tbl <- bike_orderlines_wrangled_tbl %>%
    
    # Selecting columns to focus on and adding a year column
    select(order_date, total_price) %>%
    mutate(year = year(order_date)) %>%
    
    # Grouping by year, and summarizing sales
    group_by(year) %>%
    summarize(sales = sum(total_price)) %>%
    ungroup() %>%
    
    # $ Format Text
    mutate(sales_text = scales::dollar(sales))
    
sales_by_year_tbl


# Step 2 - Visualize

sales_by_year_tbl %>%
    
    # Setup canvas with year (x-axis) and sales (y-axis)
    ggplot(aes(x = year, y = sales)) +
    
    # Geometries
    geom_col(fill = "#2c3e50") +
    geom_label(aes(label = sales_text)) +
    geom_smooth(method = "lm", se = FALSE) +
    
    # Formatting
    theme_tq() +
    scale_y_continuous(labels = scales::dollar) +
    labs(
        title = "Revenue by Year",
        subtitle = "Upward trend",
        x = "",
        y = "Revenue"
    )

# 6.2 Sales by Year and Category 2 ----


# Step 1 - Manipulate

sales_by_year_cat_2_tbl <- bike_orderlines_wrangled_tbl %>%
    
    # Selecting columns and add a year
    select(order_date, total_price, category_2) %>%
    mutate(year = year(order_date)) %>%
    
    # Groupby and Summarize year and category 2
    group_by(year, category_2) %>%
    summarise(sales = sum(total_price)) %>%
    ungroup() %>%
    
    # Format $ Text
    mutate(sales_text = scales::dollar(sales))


sales_by_year_cat_2_tbl

# Step 2 - Visualize

sales_by_year_cat_2_tbl %>%
    
    # Set up x, y, fill 
    ggplot(aes(x = year, y = sales, fill = category_2)) +
    
    # Geometries
    geom_col() +
    geom_smooth(method = "lm", se = FALSE) +
    
    # Facet
    facet_wrap(~ category_2, ncol = 3, scales = "free_y") +
    
    # Formatting
    theme_tq() +
    scale_fill_tq() +
    scale_y_continuous(labels = scales::dollar) +
    labs(
        title = "Revenue by Year and Category 2",
        subtitle = "Each product category has an upward trend",
        x = "",
        y = "Revenue",
        fill = "Product Secondary Category"
    )


# 7.0 Writing Files ----


# 7.1 Excel ----


# 7.2 CSV ----


# 7.3 RDS ----