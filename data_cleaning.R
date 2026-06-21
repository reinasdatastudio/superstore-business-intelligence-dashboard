library(lubridate)
library(readr)
library(dplyr)

superstore <- read_csv("data/Sample - Superstore.csv")

head(superstore)
summary(superstore)

superstore_clean <- superstore %>%
  # Fix date columns (some versions have inconsistent formats)
  mutate(
    Order_Date = as.Date(`Order Date`, format = "%m/%d/%Y"),
    Ship_Date = as.Date(`Ship Date`, format = "%m/%d/%Y")
  ) %>%
  # Create extra features for Shiny filters or plots
  mutate(
    Year = year(Order_Date),
    Month = month(Order_Date, label = TRUE, abbr = TRUE),
    Profit_Margin = round(Profit / Sales, 2)
  ) %>% filter(!is.na(Sales), !is.na(Profit), Sales > 0) %>%
  select(-`Order Date`, -`Ship Date`)

# 3. Save the cleaned dataset
saveRDS(superstore_clean, "superstore_clean.rds")
