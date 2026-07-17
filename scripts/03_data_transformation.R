## ============================================================
## PHASE 3: DATA TRANSFORMATION
## ============================================================
if (!requireNamespace("zoo", quietly = TRUE)) install.packages("zoo")
library(dplyr)
library(tidyr)
library(lubridate)
library(zoo)

crime_clean <- read.csv("data/crime_clean.csv", stringsAsFactors = FALSE)

month_levels <- c("January","February","March","April","May","June",
                   "July","August","September","October","November","December")
crime_clean$Month <- factor(crime_clean$Month, levels = month_levels, ordered = TRUE)

## ---- 3.1 Create a proper Date column (1st of each month) --------
crime_clean <- crime_clean %>%
  mutate(Date = as.Date(paste(Year, as.integer(Month), "01", sep = "-"), "%Y-%m-%d")) %>%
  arrange(Date, Category)

## ---- 3.2 Wide format: Category as columns (for correlation/clustering) --
crime_wide <- crime_clean %>%
  select(Date, Category, Cases) %>%
  pivot_wider(names_from = Category, values_from = Cases, values_fill = 0) %>%
  arrange(Date)

## ---- 3.3 Monthly TOTAL crime series (sum across all categories) --
monthly_total <- crime_clean %>%
  group_by(Date) %>%
  summarise(Cases = sum(Cases), .groups = "drop") %>%
  arrange(Date)

## ---- 3.4 Yearly totals per category ------------------------------
yearly_category <- crime_clean %>%
  group_by(Year, Category) %>%
  summarise(Cases = sum(Cases), .groups = "drop")

## ---- 3.5 Derived features: share of total, month index, growth ----
crime_clean <- crime_clean %>%
  group_by(Date) %>%
  mutate(Share_of_Month = Cases / sum(Cases)) %>%
  ungroup() %>%
  mutate(TimeIndex = as.integer(interval(min(Date), Date) %/% months(1)) + 1)

monthly_total <- monthly_total %>%
  mutate(
    TimeIndex   = row_number(),
    MoM_Growth  = (Cases - lag(Cases)) / lag(Cases) * 100,
    RollingMean3 = zoo::rollmean(Cases, 3, fill = NA, align = "right")
  )

## ---- 3.6 Log transform (for skewed categories like Miscellaneous) --
crime_clean <- crime_clean %>% mutate(LogCases = log1p(Cases))

## ---- 3.7 Save transformed datasets --------------------------------
write.csv(crime_clean,     "data/crime_transformed.csv", row.names = FALSE)
write.csv(crime_wide,      "data/crime_wide.csv",         row.names = FALSE)
write.csv(monthly_total,   "data/monthly_total.csv",      row.names = FALSE)
write.csv(yearly_category, "data/yearly_category.csv",    row.names = FALSE)

cat("\nPhase 3 complete: transformed datasets saved (long, wide, monthly total, yearly-by-category)\n")
