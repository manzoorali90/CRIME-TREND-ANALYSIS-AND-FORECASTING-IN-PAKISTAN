## ============================================================
## PHASE 1: DATA IMPORT
## Project: Crime Trend Analysis and Forecasting in Pakistan
##          Using Statistical and Machine Learning Techniques in R
## ============================================================
## The raw file "Moth-wise-Crime-data.xlsx" stores THREE separate
## year blocks (2022, 2023, 2024) stacked vertically in one sheet,
## each with its own "Year/Month" header row and a "Total" row/column.
## This script reads the raw sheet and reshapes it into one tidy
## long-format data frame: Year | Month | Category | Cases

## ---- 1.1 Packages -------------------------------------------
required_pkgs <- c("readxl", "dplyr", "tidyr", "stringr", "janitor", "lubridate")
new_pkgs <- required_pkgs[!(required_pkgs %in% installed.packages()[, "Package"])]
if (length(new_pkgs)) install.packages(new_pkgs)

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)
library(lubridate)

## ---- 1.2 Set working directory / paths -----------------------
## Change this path to wherever the raw file lives on your machine
raw_path <- "data/Moth-wise-Crime-data.xlsx"

## ---- 1.3 Read the raw sheet exactly as it appears -------------
raw <- read_excel(raw_path, sheet = "Sheet1", col_names = FALSE)
raw <- as.data.frame(raw)
print(dim(raw))
head(raw, 10)

## ---- 1.4 Parse the three stacked year-blocks ------------------
month_names <- c("January","February","March","April","May","June",
                  "July","August","September","October","November","December")

parse_crime_sheet <- function(raw) {
  out <- list()
  current_year <- NA
  for (i in seq_len(nrow(raw))) {
    row <- raw[i, ]

    ## a) Year marker row: a 4-digit year sits in column 7 (G)
    val_G <- suppressWarnings(as.numeric(row[[7]]))
    if (!is.na(val_G) && val_G %in% c(2022, 2023, 2024)) {
      current_year <- val_G
      next
    }

    ## b) Header row ("Year/Month") -> skip
    if (!is.na(row[[2]]) && str_trim(as.character(row[[2]])) == "Year/Month") next

    ## c) Category rows
    cat_name <- row[[2]]
    if (is.na(cat_name)) next
    cat_name <- str_trim(as.character(cat_name))
    if (cat_name == "Total" || str_detect(cat_name, "^Source")) next

    vals <- as.numeric(row[3:14])                # Jan..Dec (cols C:N)
    vals[is.na(vals)] <- 0

    out[[length(out) + 1]] <- data.frame(
      Year     = current_year,
      Month    = month_names,
      Category = cat_name,
      Cases    = vals,
      stringsAsFactors = FALSE
    )
  }
  bind_rows(out)
}

crime_long <- parse_crime_sheet(raw)

## ---- 1.5 Quick sanity check ------------------------------------
str(crime_long)
table(crime_long$Year, crime_long$Category)

## ---- 1.6 Save the tidy long-format dataset ---------------------
dir.create("data", showWarnings = FALSE)
write.csv(crime_long, "data/crime_long.csv", row.names = FALSE)

cat("\nPhase 1 complete: ", nrow(crime_long), "rows imported and saved to data/crime_long.csv\n")
