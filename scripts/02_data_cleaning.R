## ============================================================
## PHASE 2: DATA CLEANING
## ============================================================
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

crime_long <- read.csv("data/crime_long.csv", stringsAsFactors = FALSE)

## ---- 2.1 Standardize category labels (fix typos / spacing) ----
crime_long <- crime_long %>%
  mutate(
    Category = str_trim(Category),
    Category = recode(Category,
                       "Kidnapping/Abduction" = "Kidnapping/Abduction",
                       "Attempted Murder"     = "Attempted Murder",
                       "Cattle theft"         = "Cattle Theft",
                       "Other theft"          = "Other Theft")
  )

## ---- 2.2 Check & handle missing values -------------------------
sum(is.na(crime_long$Cases))                 # count of missing values
crime_long$Cases[is.na(crime_long$Cases)] <- 0

## Note: "Rape" and "Gang Rape" were only reported as separate
## categories starting in 2024 (they did not exist as distinct
## police categories in 2022-2023). We keep this as a genuine
## reporting-scope change rather than imputing false zeros for
## earlier years, and flag it for the report.
category_year_coverage <- crime_long %>%
  distinct(Year, Category) %>%
  count(Category, name = "years_reported")
print(category_year_coverage)

## ---- 2.3 Check & handle duplicate rows --------------------------
n_dupes <- sum(duplicated(crime_long))
crime_long <- distinct(crime_long)
cat("Duplicate rows removed:", n_dupes, "\n")

## ---- 2.4 Detect obvious outliers / data-entry errors -----------
## Flag any monthly value more than 3 SD from its category's mean
crime_long <- crime_long %>%
  group_by(Category) %>%
  mutate(z_score = (Cases - mean(Cases)) / sd(Cases)) %>%
  ungroup()

outliers <- crime_long %>% filter(abs(z_score) > 3)
print(outliers)   # inspect manually - none removed automatically,
                   # since these can be genuine spikes (e.g. Robbery
                   # jump in 2023, Burglary jump in Feb 2022)

crime_long <- crime_long %>% select(-z_score)

## ---- 2.5 Enforce correct data types -----------------------------
month_levels <- c("January","February","March","April","May","June",
                   "July","August","September","October","November","December")

crime_long <- crime_long %>%
  mutate(
    Year     = as.integer(Year),
    Month    = factor(Month, levels = month_levels, ordered = TRUE),
    Category = as.factor(Category),
    Cases    = as.numeric(Cases)
  )

## ---- 2.6 Save cleaned dataset -----------------------------------
write.csv(crime_long, "data/crime_clean.csv", row.names = FALSE)

cat("\nPhase 2 complete: cleaned dataset saved to data/crime_clean.csv\n")
cat("Rows:", nrow(crime_long), " | Categories:", length(unique(crime_long$Category)), "\n")
