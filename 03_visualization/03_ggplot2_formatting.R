# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# FORMATTING GGPLOTS ----

# Libraries & Data ----

library(tidyverse)
library(lubridate)
library(tidyquant)

bike_orderlines_tbl <- read_rds("00_data/bike_sales/data_wrangled/bike_orderlines.rds")

glimpse(bike_orderlines_tbl)

# Data Manipulation

sales_by_year_category_2_tbl <- bike_orderlines_tbl %>%
    select(order_date, category_2, total_price) %>%
    
    mutate(order_date = ymd(order_date)) %>%
    mutate(year = year(order_date)) %>%
    
    group_by(category_2, year) %>%
    summarize(revenue = sum(total_price)) %>%
    ungroup() %>%
    
    mutate(category_2 = fct_reorder2(category_2, year, revenue))

sales_by_year_category_2_tbl

sales_by_year_category_2_tbl %>%
    mutate(category_2_num = as.numeric(category_2)) %>%
    arrange(category_2_num)

# 1.0 Working with Colors ----

# 1.1 Color Conversion ----

# Named Colors
colors()

sales_by_year_category_2_tbl %>%
    
    ggplot(aes(year, revenue)) +
    
    geom_col(fill = viridisLite::viridis(n = 20)[10])

# To RGB
col2rgb("slateblue")

col2rgb("#2C3E50")

# To HEX
rgb(44, 62, 80, maxColorValue = 255)


# 1.2 Color Palettes ----

# tidyquant
tidyquant::palette_light()

palette_light()[2] %>% col2rgb()

# Brewer
RColorBrewer::display.brewer.all()
RColorBrewer::brewer.pal.info
RColorBrewer::brewer.pal(n = 8, name = "Blues")

# Viridis
viridisLite::viridis(n = 20)



# 2.0 Aesthetic Mappings ----

# 2.1 Color  -----
# - Used with line and points, Outlines of rectangular objects
sales_by_year_category_2_tbl %>%
    
    ggplot(aes(year, revenue, color = category_2)) +
    geom_line(size = 1) +
    geom_point(color = "dodgerblue", size = 5)


# 2.2 Fill  -----
# - Used with fill of rectangular objects 

sales_by_year_category_2_tbl %>%
    
    ggplot(aes(year, revenue)) +
    geom_col(aes(fill = category_2))

# 2.3 Size ----
# - Used with points

sales_by_year_category_2_tbl %>%
    
    ggplot(aes(year, revenue, size = revenue)) +
    geom_line(aes(color = category_2), size = 1) +
    geom_point()



# 3.0 Faceting ----
# - Great way to tease out variation by category

# Goal: Sales annual sales by category 2

sales_by_year_category_2_tbl %>%
    
    ggplot(aes(year, revenue, color = category_2)) +
    geom_line(color = "black") +
    geom_smooth(method = "lm", se = FALSE) +
    
    facet_wrap(~ category_2, ncol = 3, scales = "free_y") +
    
    expand_limits(y = 0)


# 4.0 Position Adjustments (Stack & Dodge) ----

# Stacked Bars & Side-By-Side Bars

sales_by_year_category_2_tbl %>%
    
    ggplot(aes(year, revenue, fill = category_2)) +
    # geom_col(position = "stack")
    # geom_col(position = "dodge")
    geom_col(position = position_dodge(width = 0.9), color = "white")

# Stacked Area

sales_by_year_category_2_tbl %>%
    
    ggplot(aes(year, revenue, fill = category_2)) +
    geom_area(color = "black")




# 5.0 Scales (Colors, Fills, Axis) ----

# 5.1 Plot Starting Points ----
# - Continuous (e.g. Revenue): Changes color via gradient palette
# - Categorical (e.g. ): Changes color via discrete palette

# Plot 1: Faceted Plot, Color = Continuous Scale
g_facet_continuous <- sales_by_year_category_2_tbl %>%
    
    ggplot(aes(year, revenue, color = revenue)) +
    geom_line(size = 1) +
    geom_point(size = 3) +
    
    facet_wrap(~ category_2, scales = "free_y") +
    expand_limits(y = 0) +
    
    theme_minimal()

g_facet_continuous


# Plot 2: Faceted Plot, Color = Discrete Scale
g_facet_discrete <- sales_by_year_category_2_tbl %>%
    
    ggplot(aes(year, revenue, color = category_2)) +
    geom_line(size = 1) +
    geom_point(size = 3) +
    
    facet_wrap(~ category_2, scales = "free_y") +
    expand_limits(y = 0) +
    
    theme_minimal()

g_facet_discrete

# Plot 3: Stacked Area Plot
g_area_discrete <- sales_by_year_category_2_tbl %>%
    
    ggplot(aes(year, revenue, fill = category_2)) +
    geom_area(color = "black") +
    
    theme_minimal()

g_area_discrete

# 5.2 Scale Colors & Fills ----
# - Awesome way to show variation by groups (discrete) and by values (continuous)

# Color by Revenue (Continuous Scale)

g_facet_continuous +
    
    # scale_color_continuous(
    #     low   = "black",
    #     high  = "cornflowerblue" 
    # )
    scale_color_viridis_c(option = "E", direction = -1)



# Color by Category 2 (Discrete Scale)
RColorBrewer::display.brewer.all()
RColorBrewer::brewer.pal.info
RColorBrewer::brewer.pal(n = 8, name = "Blues")

g_facet_discrete +
    scale_color_brewer(palette = "Set3") +
    theme_dark()

g_facet_discrete +
    scale_color_tq(theme = "dark") +
    theme_dark()

g_facet_discrete +
    scale_color_viridis_d(option = "D") +
    theme_dark()


# Fill by Category 2

g_area_discrete +
    scale_fill_brewer(palette = "Set3")

g_area_discrete +
    scale_fill_tq()

g_area_discrete +
    scale_fill_viridis_d()


# 5.3 Axis Scales ----

sales_by_year_category_2_tbl

g_facet_continuous +
    scale_x_continuous(breaks = seq(2011, 2015, by = 2)) +
    scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M"))



# 6.0 Labels ----

g_facet_continuous +
    
    scale_x_continuous(breaks = seq(2011, 2015, by = 2)) +
    scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M")) +
    
    geom_smooth(method = "lm", se = FALSE) +
    
    scale_color_viridis_c() +
    theme_dark() +
    
    labs(
        title = "Bike Sales",
        subtitle = "Sales are trending up",
        caption = "5-year sales trends\ncomes from our ERP Database",
        x = "Year",
        y = "Revenue ($M)",
        color = "Revenue"
    )


# 7.0 Themes  ----

g_facet_continuous +
    
    theme_light() +
    
    theme(
        axis.text.x = element_text(
            angle = 45, 
            hjust = 1
        ),
        strip.background = element_rect(
            color = "black",
            fill  = "cornflowerblue",
            size  = 1
        ),
        strip.text = element_text(
            face  = "bold",
            color = "white"
        )
    )


# 8.0 Putting It All Together ----

# Brewer
RColorBrewer::display.brewer.all()
RColorBrewer::brewer.pal.info
RColorBrewer::brewer.pal(n = 9, name = "Blues")

sales_by_year_category_2_tbl %>%
    
    ggplot(aes(year, revenue, fill = category_2)) +
    
    geom_area(color = "black") +
    
    # Scales
    scale_fill_brewer(palette = "Blues", direction = -1) +
    scale_y_continuous(labels = scales::dollar_format()) +
    
    # Labels
    labs(
        title = "Sales Over Year by Category 2",
        subtitle = "Sales Trending Upward",
        x = "",
        y = "Revenue ($M)",
        fill = "2nd Category",
        caption = "Bike sales trends look strong heading into 2016"
    ) +
    
    # Theme
    theme_light() +
    theme(
        title = element_text(face = "bold", color = "#08306B")
        
    )




