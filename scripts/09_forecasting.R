## ============================================================
## PHASE 9: FORECASTING (ARIMA & ETS)
## ============================================================
if (!requireNamespace("forecast", quietly = TRUE)) install.packages("forecast")
library(forecast)
library(ggplot2)

crime_ts <- readRDS("data/crime_ts.rds")

## ---- 9.1 Train/test split (hold out the last 6 months) -----------------
h <- 6
train <- window(crime_ts, end = c(2024, 6))
test  <- window(crime_ts, start = c(2024, 7))

## ---- 9.2 Fit ARIMA (auto.arima selects the best order by AIC) ----------
arima_model <- auto.arima(train, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)
print(summary(arima_model))
arima_fc <- forecast(arima_model, h = h)

## ---- 9.3 Fit ETS (Holt-Winters / exponential smoothing) -----------------
ets_model <- ets(train)
print(summary(ets_model))
ets_fc <- forecast(ets_model, h = h)

## ---- 9.4 Accuracy comparison on the holdout set --------------------------
arima_acc <- accuracy(arima_fc, test)
ets_acc   <- accuracy(ets_fc, test)

cat("=== ARIMA Accuracy (Test Set) ===\n"); print(arima_acc)
cat("\n=== ETS Accuracy (Test Set) ===\n"); print(ets_acc)

comparison <- data.frame(
  Model = c("ARIMA", "ETS"),
  RMSE  = c(arima_acc["Test set", "RMSE"], ets_acc["Test set", "RMSE"]),
  MAE   = c(arima_acc["Test set", "MAE"],  ets_acc["Test set", "MAE"]),
  MAPE  = c(arima_acc["Test set", "MAPE"], ets_acc["Test set", "MAPE"])
)
print(comparison)
write.csv(comparison, "data/forecast_accuracy_comparison.csv", row.names = FALSE)

## ---- 9.5 Plot forecasts against actuals -----------------------------------
png("figures/21_arima_forecast.png", width = 800, height = 450)
plot(arima_fc, main = "ARIMA Forecast of Total Monthly Crime")
lines(test, col = "black", lwd = 2, lty = 2)
legend("topleft", legend = c("Forecast", "Actual (holdout)"), col = c("blue","black"), lty = c(1,2))
dev.off()

png("figures/22_ets_forecast.png", width = 800, height = 450)
plot(ets_fc, main = "ETS (Holt-Winters) Forecast of Total Monthly Crime")
lines(test, col = "black", lwd = 2, lty = 2)
legend("topleft", legend = c("Forecast", "Actual (holdout)"), col = c("blue","black"), lty = c(1,2))
dev.off()

## ---- 9.6 Refit best model on FULL data and forecast forward (next 6 months) --
best_model_name <- comparison$Model[which.min(comparison$MAPE)]
cat("\nBest performing model on holdout accuracy:", best_model_name, "\n")

full_arima <- auto.arima(crime_ts, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)
future_fc  <- forecast(full_arima, h = 6)
print(future_fc)

png("figures/34_future_forecast.png", width = 800, height = 450)
plot(future_fc, main = "6-Month Ahead Forecast of Total Monthly Crime (Final Model)")
dev.off()

write.csv(data.frame(Date = as.character(time(future_fc$mean)),
                      Forecast = as.numeric(future_fc$mean),
                      Lower95  = as.numeric(future_fc$lower[,2]),
                      Upper95  = as.numeric(future_fc$upper[,2])),
          "data/future_forecast_6months.csv", row.names = FALSE)

cat("\nPhase 9 complete: ARIMA & ETS models fit, compared, and 6-month forecast saved\n")
cat("Best model:", best_model_name, "| ARIMA order:", paste(arimaorder(full_arima), collapse=","), "\n")
