## ============================================================
## PHASE 4: EXPLORATORY DATA ANALYSIS (20+ graphs)
## ============================================================
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

crime <- read.csv("data/crime_transformed.csv", stringsAsFactors = FALSE)
monthly_total <- read.csv("data/monthly_total.csv", stringsAsFactors = FALSE)
yearly_category <- read.csv("data/yearly_category.csv", stringsAsFactors = FALSE)

month_levels <- c("January","February","March","April","May","June",
                   "July","August","September","October","November","December")
crime$Month <- factor(crime$Month, levels = month_levels, ordered = TRUE)
crime$Date  <- as.Date(crime$Date)
monthly_total$Date <- as.Date(monthly_total$Date)

main_cats <- c("Murder","Attempted Murder","Kidnapping/Abduction","Dacoity",
               "Robbery","Burglary","Cattle Theft","Other Theft","Miscellaneous")

dir.create("figures", showWarnings = FALSE)
theme_set(theme_minimal(base_size = 11))
save_plot <- function(p, name, w = 8, h = 5) ggsave(file.path("figures", name), p, width = w, height = h, dpi = 150)

## 01. Total monthly crime trend
p1 <- ggplot(monthly_total, aes(Date, Cases)) +
  geom_line(color = "#1f4e79") + geom_point(color = "#1f4e79") +
  labs(title = "Total Monthly Crime Cases in Pakistan (2022-2024)", x = "Month", y = "Cases")
save_plot(p1, "01_total_trend.png")

## 02. Yearly totals bar chart
yearly_totals <- crime %>% group_by(Year) %>% summarise(Cases = sum(Cases))
p2 <- ggplot(yearly_totals, aes(factor(Year), Cases)) +
  geom_col(fill = "#2e7d32") + labs(title = "Total Reported Crimes by Year", x = "Year", y = "Cases")
save_plot(p2, "02_yearly_totals.png", 6, 4)

## 03. Category totals (all categories)
cat_totals <- crime %>% group_by(Category) %>% summarise(Cases = sum(Cases)) %>% arrange(desc(Cases))
p3 <- ggplot(cat_totals, aes(reorder(Category, Cases), Cases)) +
  geom_col(fill = "#6a1b9a") + coord_flip() +
  labs(title = "Total Cases by Crime Category (2022-2024)", x = NULL, y = "Cases")
save_plot(p3, "03_category_totals.png", 8, 5)

## 04. Zoomed category totals (excl. Miscellaneous & Other Theft)
p4 <- cat_totals %>% filter(!Category %in% c("Miscellaneous","Other Theft")) %>%
  ggplot(aes(reorder(Category, Cases), Cases)) + geom_col(fill = "#c62828") + coord_flip() +
  labs(title = "Category Totals (Violent & Property Crimes, Excl. Theft/Misc)", x = NULL, y = "Cases")
save_plot(p4, "04_category_zoom.png", 8, 5)

## 05. Seasonality boxplot: total by calendar month
month_totals <- crime %>% group_by(Year, Month) %>% summarise(Cases = sum(Cases), .groups = "drop")
p5 <- ggplot(month_totals, aes(Month, Cases)) + geom_boxplot(fill = "#0288d1") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Seasonality: Distribution of Total Crime by Calendar Month")
save_plot(p5, "05_month_boxplot.png", 9, 4.5)

## 06. Heatmap: Year x Month
p6 <- ggplot(month_totals, aes(Month, factor(Year), fill = Cases)) + geom_tile() +
  scale_fill_gradient(low = "#fff5eb", high = "#c62828") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Heatmap of Total Crime by Year and Month", y = "Year")
save_plot(p6, "06_heatmap_year_month.png", 9, 3.5)

## 07. Category trend lines (top 5 excl. misc/other theft)
top5 <- c("Robbery","Burglary","Kidnapping/Abduction","Cattle Theft","Attempted Murder")
p7 <- crime %>% filter(Category %in% top5) %>%
  ggplot(aes(Date, Cases, color = Category)) + geom_line() + geom_point(size = 0.8) +
  labs(title = "Monthly Trend by Crime Category")
save_plot(p7, "07_category_trends.png", 9, 5)

## 08. Category share pie chart (2024)
d2024 <- crime %>% filter(Year == 2024) %>% group_by(Category) %>% summarise(Cases = sum(Cases))
p8 <- ggplot(d2024, aes(x = "", y = Cases, fill = Category)) +
  geom_col(width = 1) + coord_polar("y") +
  labs(title = "Crime Category Share in 2024") + theme_void()
save_plot(p8, "08_pie_2024.png", 6, 6)

## 09. Correlation heatmap between categories
wide <- crime %>% filter(Category %in% main_cats) %>%
  select(Date, Category, Cases) %>% pivot_wider(names_from = Category, values_from = Cases)
corr_mat <- cor(wide[, main_cats])
corr_df <- as.data.frame(as.table(corr_mat))
p9 <- ggplot(corr_df, aes(Var1, Var2, fill = Freq)) + geom_tile() +
  geom_text(aes(label = round(Freq, 2)), size = 2.5) +
  scale_fill_gradient2(low = "#1f4e79", mid = "white", high = "#c62828", midpoint = 0) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Between Crime Categories", x = NULL, y = NULL, fill = "r")
save_plot(p9, "09_correlation_heatmap.png", 7, 6)

## 10. Histogram of total monthly cases
p10 <- ggplot(monthly_total, aes(Cases)) + geom_histogram(bins = 10, fill = "#0288d1", color = "white") +
  labs(title = "Distribution of Total Monthly Crime Cases")
save_plot(p10, "11_histogram.png", 7, 4)

## 11. Year-on-year growth
yoy <- yearly_totals %>% arrange(Year) %>% mutate(Growth = (Cases - lag(Cases)) / lag(Cases) * 100)
p11 <- ggplot(na.omit(yoy), aes(factor(Year), Growth)) + geom_col(fill = "#ef6c00") +
  labs(title = "Year-on-Year Growth in Total Crime (%)", x = "Year", y = "% change")
save_plot(p11, "12_yoy_growth.png", 6, 4)

## 12. Category growth 2022 -> 2024
g <- yearly_category %>% filter(Category %in% main_cats) %>%
  pivot_wider(names_from = Year, values_from = Cases) %>%
  mutate(Growth = (`2024` - `2022`) / `2022` * 100)
p12 <- ggplot(g, aes(reorder(Category, Growth), Growth, fill = Growth > 0)) + geom_col() + coord_flip() +
  scale_fill_manual(values = c("#2e7d32","#c62828"), guide = "none") +
  labs(title = "% Change in Category Totals: 2022 to 2024", x = NULL, y = "% change")
save_plot(p12, "13_category_growth.png", 8, 5)

## 13. Stacked area of category share over time
share <- wide %>% pivot_longer(-Date, names_to = "Category", values_to = "Cases") %>%
  group_by(Date) %>% mutate(Share = Cases / sum(Cases)) %>% ungroup()
p13 <- ggplot(share, aes(Date, Share, fill = Category)) + geom_area() +
  labs(title = "Category Share of Total Crime Over Time", y = "Proportion")
save_plot(p13, "14_stacked_share.png", 9, 5)

## 14. Boxplot of monthly cases by category (log scale)
p14 <- crime %>% filter(Category %in% main_cats) %>%
  ggplot(aes(Cases, reorder(Category, Cases, median))) + geom_boxplot(fill = "#8e24aa") +
  scale_x_log10() + labs(title = "Distribution of Monthly Cases by Category (log scale)", y = NULL)
save_plot(p14, "15_boxplot_category_log.png", 9, 5)

## 15. Scatter: Robbery vs Burglary with regression line
p15 <- ggplot(wide, aes(Robbery, `Burglary`)) + geom_point(color = "#1f4e79") +
  geom_smooth(method = "lm", color = "#c62828", se = TRUE) +
  labs(title = "Robbery vs Burglary (Monthly Totals)")
save_plot(p15, "16_scatter_robbery_burglary.png", 6, 5)

## 16. 3-month moving average
monthly_total <- monthly_total %>% arrange(Date) %>%
  mutate(MA3 = zoo::rollmean(Cases, 3, fill = NA, align = "right"))
p16 <- ggplot(monthly_total, aes(Date)) +
  geom_line(aes(y = Cases), color = "grey70") +
  geom_line(aes(y = MA3), color = "#c62828", linewidth = 1) +
  labs(title = "Total Crime with 3-Month Moving Average", y = "Cases")
save_plot(p16, "17_moving_average.png", 8, 4)

## 17. Density plot by category (top categories, excl. misc)
p17 <- crime %>% filter(Category %in% top5) %>%
  ggplot(aes(Cases, fill = Category)) + geom_density(alpha = 0.4) +
  labs(title = "Density of Monthly Cases by Category")
save_plot(p17, "18b_density_by_category.png", 8, 5)

## 18. Violin plot: Cases by Year (main categories)
p18 <- crime %>% filter(Category %in% main_cats) %>%
  ggplot(aes(factor(Year), Cases)) + geom_violin(fill = "#00838f", alpha = 0.7) +
  scale_y_log10() + labs(title = "Distribution of Cases by Year (log scale)", x = "Year")
save_plot(p18, "24_violin_year.png", 6, 5)

## 19. Faceted trend: all main categories
p19 <- crime %>% filter(Category %in% main_cats) %>%
  ggplot(aes(Date, Cases)) + geom_line(color = "#1f4e79") +
  facet_wrap(~Category, scales = "free_y") +
  labs(title = "Monthly Trend for Every Crime Category")
save_plot(p19, "25_facet_categories.png", 10, 7)

## 20. Bar chart: average monthly cases by category
p20 <- crime %>% filter(Category %in% main_cats) %>% group_by(Category) %>%
  summarise(Avg = mean(Cases)) %>%
  ggplot(aes(reorder(Category, Avg), Avg)) + geom_col(fill = "#455a64") + coord_flip() +
  labs(title = "Average Monthly Cases by Category", x = NULL, y = "Average Cases")
save_plot(p20, "26_avg_cases_bar.png", 8, 5)

## 21. Scatter: Time index vs Total Cases (for regression intuition)
monthly_total <- monthly_total %>% mutate(TimeIndex = row_number())
p21 <- ggplot(monthly_total, aes(TimeIndex, Cases)) + geom_point(color="#1f4e79") +
  geom_smooth(method = "lm", color = "#c62828") +
  labs(title = "Total Crime vs Time Index (Linear Trend)", x = "Month Index (1-36)")
save_plot(p21, "27_trend_scatter.png", 7, 5)

## 22. Cumulative crime over time
monthly_total <- monthly_total %>% mutate(Cumulative = cumsum(Cases))
p22 <- ggplot(monthly_total, aes(Date, Cumulative)) + geom_area(fill = "#0288d1", alpha = 0.6) +
  labs(title = "Cumulative Total Crime Cases (2022-2024)", y = "Cumulative Cases")
save_plot(p22, "28_cumulative.png", 8, 4)

## 23. Category totals faceted by year (bar)
p23 <- yearly_category %>% filter(Category %in% main_cats) %>%
  ggplot(aes(reorder(Category, Cases), Cases)) + geom_col(fill = "#8d6e63") + coord_flip() +
  facet_wrap(~Year) + labs(title = "Category Totals by Year", x = NULL)
save_plot(p23, "29_category_by_year.png", 10, 5)

cat("\nPhase 4 complete: 20+ EDA charts saved to figures/\n")
