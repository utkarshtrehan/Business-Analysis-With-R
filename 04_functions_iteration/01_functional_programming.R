# DS4B 101-R: R FOR BUSINESS ANALYSIS ---- 
# FUNCTIONAL PROGRAMMING ----


install.packages("ggrepel") # ggrepel needed for text and label repel in plots

library(tidyverse)
library(lubridate)
library(tidyquant)
library(ggrepel)
library(fs)

bike_orderlines_tbl <- read_rds("00_data/bike_sales/data_wrangled/bike_orderlines.rds")

glimpse(bike_orderlines_tbl)





# 1.0 ANATOMY OF A FUNCTION ----

# 1.1 Examining the mean() function ----
x <- c(0:10, 50, NA_real_)
x



# 1.2 Customizing a mean function ----

# Name                     # Arguments
mean_remove_na <- function(x, na.rm = TRUE, ...) {
    
    # Body
    avg <- mean(x, na.rm = na.rm, ...)
    
    # Return
    return(avg)
}








# 2.0 THE TWO STYLES OF FUNCTIONS: VECTOR FUNCTIONS & DATA FUNCTIONS ----

# Calculating a 3 month rolling average  for category_1 & category_2 
# with dates aligned at last day of the month




# 2.1 Vector Functions ----


# 2.2 Data Functions ----


# 3.0 CONTROLLING FLOW: IF STATEMENTS, MESSAGES, WARNINGS, STOP ----


# 4.0 VECTORIZED REMOVE OUTLIERS FUNCTION ----
#  - Box Plot Diagram to Identify Outliers
#  - Goal: Use box plot approach to identify outliers

# Make bikes_tbl



# Visualize Box Plot



# Create remove_outliers()



# Apply remove_outliers() to bikes_tbl



# Visualize with remove_outlers()





# 5.0 DATA FUNCTION: FEATURE ENGINEERING ----
#  - Goal: Want to simplify the text feature engineering steps to convert model name to features



# Pipeline Comes From 02_data_wrangling/04_text.R
bikes_tbl %>%
    
    select(model) %>%
    
    # Fix typo
    mutate(model = case_when(
        model == "CAAD Disc Ultegra" ~ "CAAD12 Disc Ultegra",
        model == "Syapse Carbon Tiagra" ~ "Synapse Carbon Tiagra",
        model == "Supersix Evo Hi-Mod Utegra" ~ "Supersix Evo Hi-Mod Ultegra",
        TRUE ~ model
    )) %>%
    
    # separate using spaces
    separate(col     = model, 
             into    = str_c("model_", 1:7), 
             sep     = " ", 
             remove  = FALSE, 
             fill    = "right") %>%
    
    # creating a "base" feature
    mutate(model_base = case_when(
        
        # Fix Supersix Evo
        str_detect(str_to_lower(model_1), "supersix") ~ str_c(model_1, model_2, sep = " "),
        
        # Fix Fat CAAD bikes
        str_detect(str_to_lower(model_1), "fat") ~ str_c(model_1, model_2, sep = " "),
        
        # Fix Beast of the East
        str_detect(str_to_lower(model_1), "beast") ~ str_c(model_1, model_2, model_3, model_4, sep = " "),
        
        # Fix Bad Habit
        str_detect(str_to_lower(model_1), "bad") ~ str_c(model_1, model_2, sep = " "),
        
        # Fix Scalpel 29
        str_detect(str_to_lower(model_2), "29") ~ str_c(model_1, model_2, sep = " "),
        
        # catch all
        TRUE ~ model_1)
    ) %>%
    
    # Get "tier" feature
    mutate(model_tier = model %>% str_replace(model_base, replacement = "") %>% str_trim()) %>%
    
    # Remove unnecessary columns
    select(-matches("[0-9]")) %>%
    
    # Create Flags
    mutate(
        black     = model_tier %>% str_to_lower() %>% str_detect("black") %>% as.numeric(),
        hi_mod    = model_tier %>% str_to_lower() %>% str_detect("hi-mod") %>% as.numeric(),
        team      = model_tier %>% str_to_lower() %>% str_detect("team") %>% as.numeric(),
        red       = model_tier %>% str_to_lower() %>% str_detect("red") %>% as.numeric(),
        ultegra   = model_tier %>% str_to_lower() %>% str_detect("ultegra") %>% as.numeric(),
        dura_ace  = model_tier %>% str_to_lower() %>% str_detect("dura ace") %>% as.numeric(),
        disc      = model_tier %>% str_to_lower() %>% str_detect("disc") %>% as.numeric()
    )







# 6.0 SAVING AND SOURCING FUNCTIONS ----

# 6.1 Create folder and file ----



# 6.2 Build and add header ----



# 6.3 Add functions with dump() ----



# 6.4 Source function ----
