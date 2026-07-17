## ============================================================
## PHASE 5: STATISTICAL ANALYSIS
## ============================================================
library(dplyr)
library(tidyr)

crime <- read.csv("data/crime_transformed.csv", stringsAsFactors = FALSE)
main_cats <- c("Murder","Attempted Murder","Kidnapping/Abduction","Dacoity",
               "Robbery","Burglary","Cattle Theft","Other Theft","Miscellaneous")

## ---- 5.1 Descriptive statistics per category ----------------------
desc_stats <- crime %>%
  group_by(Category) %>%
  summarise(
    n      = n(),
    mean   = mean(Cases),
    sd     = sd(Cases),
    min    = min(Cases),
    median = median(Cases),
    max    = max(Cases),
    total  = sum(Cases),
    cv     = sd / mean            # coefficient of variation
  ) %>% arrange(desc(total))

print(desc_stats)
write.csv(desc_stats, "data/descriptive_stats.csv", row.names = FALSE)

## ---- 5.2 One-way ANOVA: does Year affect case counts? --------------
## (restricted to categories present in all 3 years)
anova_data <- crime %>% filter(Category %in% main_cats)
anova_model <- aov(Cases ~ factor(Year), data = anova_data)
print(summary(anova_model))
## Interpretation: tests whether mean monthly case counts differ
## significantly by year, pooling across all main categories.

## ---- 5.3 One-way ANOVA: does Month (seasonality) affect totals? ----
month_totals <- crime %>% group_by(Year, Month) %>% summarise(Cases = sum(Cases), .groups = "drop")
anova_month <- aov(Cases ~ Month, data = month_totals)
print(summary(anova_month))

## ---- 5.4 Correlation matrix + significance tests --------------------
wide <- crime %>% filter(Category %in% main_cats) %>%
  select(Date, Category, Cases) %>% pivot_wider(names_from = Category, values_from = Cases)

corr_mat <- cor(wide[, main_cats])
print(round(corr_mat, 2))

## Pairwise correlation test example: Robbery vs Burglary
ct <- cor.test(wide$Robbery, wide$Burglary)
print(ct)

## ---- 5.5 Normality check (Shapiro-Wilk) on total monthly series -----
monthly_total <- read.csv("data/monthly_total.csv", stringsAsFactors = FALSE)
shapiro_result <- shapiro.test(monthly_total$Cases)
print(shapiro_result)

## ---- 5.6 Save summary statistical report -----------------------------
sink("data/statistical_analysis_summary.txt")
cat("=== Descriptive Statistics by Category ===\n"); print(desc_stats)
cat("\n=== ANOVA: Cases ~ Year ===\n"); print(summary(anova_model))
cat("\n=== ANOVA: Cases ~ Month (seasonality) ===\n"); print(summary(anova_month))
cat("\n=== Correlation Matrix ===\n"); print(round(corr_mat, 2))
cat("\n=== Shapiro-Wilk Normality Test (Total Monthly Series) ===\n"); print(shapiro_result)
sink()

cat("\nPhase 5 complete: statistical summary saved to data/statistical_analysis_summary.txt\n")
