## ============================================================
## PHASE 6: REGRESSION ANALYSIS
## ============================================================
library(dplyr)
library(ggplot2)

monthly_total <- read.csv("data/monthly_total.csv", stringsAsFactors = FALSE)
monthly_total$Date <- as.Date(monthly_total$Date)
monthly_total <- monthly_total %>% arrange(Date) %>% mutate(TimeIndex = row_number(),
                                                              MonthNum = as.integer(format(Date, "%m")))

## ---- 6.1 Simple linear trend model: Cases ~ Time -----------------
lm_trend <- lm(Cases ~ TimeIndex, data = monthly_total)
print(summary(lm_trend))
## Slope tells you the average monthly increase in total crime cases.

## ---- 6.2 Multiple regression: trend + seasonal month effect -------
lm_full <- lm(Cases ~ TimeIndex + factor(MonthNum), data = monthly_total)
print(summary(lm_full))

## ---- 6.3 Model diagnostics -----------------------------------------
par(mfrow = c(2,2))
plot(lm_trend)
par(mfrow = c(1,1))

## ---- 6.4 Predicted vs actual plot -----------------------------------
monthly_total$Predicted <- predict(lm_trend)
p <- ggplot(monthly_total, aes(Date)) +
  geom_point(aes(y = Cases), color = "#1f4e79") +
  geom_line(aes(y = Predicted), color = "#c62828", linewidth = 1) +
  labs(title = "Linear Trend Regression: Actual vs Fitted", y = "Cases") +
  theme_minimal()
ggsave("figures/30_regression_fit.png", p, width = 8, height = 5, dpi = 150)

## ---- 6.5 Category-level regression example (Robbery ~ Time) --------
crime <- read.csv("data/crime_transformed.csv", stringsAsFactors = FALSE)
robbery <- crime %>% filter(Category == "Robbery") %>% arrange(Date) %>% mutate(TimeIndex = row_number())
lm_robbery <- lm(Cases ~ TimeIndex, data = robbery)
print(summary(lm_robbery))

## ---- 6.6 Save regression outputs ------------------------------------
sink("data/regression_summary.txt")
cat("=== Model 1: Cases ~ TimeIndex (overall trend) ===\n"); print(summary(lm_trend))
cat("\n=== Model 2: Cases ~ TimeIndex + Month (trend + seasonality) ===\n"); print(summary(lm_full))
cat("\n=== Model 3: Robbery Cases ~ TimeIndex ===\n"); print(summary(lm_robbery))
sink()

cat("\nPhase 6 complete: regression results saved to data/regression_summary.txt\n")
cat("R-squared (trend only):", summary(lm_trend)$r.squared, "\n")
cat("R-squared (trend+seasonal):", summary(lm_full)$r.squared, "\n")
