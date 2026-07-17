## ============================================================
## PHASE 8: TIME SERIES ANALYSIS
## ============================================================
if (!requireNamespace("tseries", quietly = TRUE)) install.packages("tseries")
library(dplyr)
library(ggplot2)
library(tseries)

monthly_total <- read.csv("data/monthly_total.csv", stringsAsFactors = FALSE)
monthly_total$Date <- as.Date(monthly_total$Date)
monthly_total <- monthly_total %>% arrange(Date)

## ---- 8.1 Convert to a ts object (monthly, starting Jan 2022) ----------
crime_ts <- ts(monthly_total$Cases, start = c(2022, 1), frequency = 12)
print(crime_ts)

## ---- 8.2 Plot the raw series --------------------------------------------
png("figures/32_ts_plot.png", width = 800, height = 400)
plot(crime_ts, main = "Total Monthly Crime Cases (Time Series)", ylab = "Cases", xlab = "Year")
dev.off()

## ---- 8.3 Seasonal decomposition (additive) -------------------------------
decomp <- decompose(crime_ts, type = "additive")
png("figures/10_decomposition.png", width = 800, height = 700)
plot(decomp)
dev.off()

## ---- 8.4 Stationarity testing -------------------------------------------
## Augmented Dickey-Fuller test on raw series
adf_raw <- adf.test(crime_ts)
print(adf_raw)

## ADF test after first differencing
diff_ts <- diff(crime_ts)
adf_diff <- adf.test(diff_ts)
print(adf_diff)

png("figures/33_differenced_series.png", width = 800, height = 400)
plot(diff_ts, main = "First-Differenced Total Crime Series", ylab = "Change in Cases")
abline(h = 0, col = "red", lty = 2)
dev.off()

## ---- 8.5 ACF / PACF plots (to inform ARIMA order in Phase 9) -------------
png("figures/20_acf_pacf.png", width = 900, height = 400)
par(mfrow = c(1, 2))
acf(diff_ts, main = "ACF of Differenced Series")
pacf(diff_ts, main = "PACF of Differenced Series")
par(mfrow = c(1, 1))
dev.off()

## ---- 8.6 Save time series diagnostics summary -----------------------------
sink("data/timeseries_summary.txt")
cat("=== ADF Test on Raw Series ===\n"); print(adf_raw)
cat("\n=== ADF Test on First-Differenced Series ===\n"); print(adf_diff)
cat("\nInterpretation: a p-value < 0.05 on the differenced series confirms\n")
cat("the series becomes stationary after first differencing (d = 1),\n")
cat("which justifies the ARIMA(p,1,q) family used in Phase 9.\n")
sink()

## save the ts object + decomposition for use in Phase 9
saveRDS(crime_ts, "data/crime_ts.rds")

cat("\nPhase 8 complete: decomposition, stationarity tests, and ACF/PACF plots saved\n")
