# CRIME-TREND-ANALYSIS-AND-FORECASTING-IN-PAKISTAN
CRIME TREND ANALYSIS AND FORECASTING IN PAKISTAN, A Statistical and Machine Learning Approach Using ARIMA and ETS Models
# Crime Trend Analysis and Forecasting in Pakistan

A Statistical and Machine Learning Approach Using R (ARIMA & ETS Models)

## 📌 Overview
This project analyzes month-wise crime data across Pakistan (2022–2024) to uncover trends, seasonal patterns, and category-level relationships in reported crime, and builds time-series models to forecast future crime levels. The full analysis was conducted in **R**, covering data cleaning, exploratory data analysis, statistical testing, regression, clustering, and forecasting.

**Data source:** Ministry of Interior, National Police Bureau, Islamabad
**Time span:** January 2022 – December 2024
**Crime categories:** 11 (Murder, Attempted Murder, Kidnapping/Abduction, Dacoity, Robbery, Burglary, Cattle Theft, Other Theft, Miscellaneous, Rape, Gang Rape)

## 🎯 Objectives
- Clean and structure raw, irregularly formatted government crime data
- Explore trends and seasonality across crime categories
- Test statistical relationships between crime types
- Cluster crime categories by behavioral similarity
- Forecast future crime levels using ARIMA and ETS (Holt-Winters) models

## 🛠️ Methodology
| Phase | Description |
|-------|-------------|
| 1. Data Import | Parsed raw multi-year Excel sheet into a tidy long-format dataset |
| 2. Data Cleaning | Handled missing values, duplicates, inconsistent labels, outliers |
| 3. Data Transformation | Created wide/long formats, monthly totals, log transforms, rolling averages |
| 4. Exploratory Data Analysis | 20+ visualizations (trends, seasonality, heatmaps, distributions) |
| 5. Statistical Analysis | Descriptive stats, ANOVA, correlation analysis |
| 6. Regression Analysis | Trend and seasonal regression on total monthly crime |
| 7. Clustering | K-means clustering of crime categories by monthly pattern |
| 8. Time Series Analysis | Seasonal decomposition, stationarity testing (ADF) |
| 9. Forecasting | ARIMA and ETS models, compared via RMSE/MAPE on holdout data |
| 10. Report | Full write-up of methodology, findings, and forecasts |

## 📊 Key Findings
- Total reported crime rose **~31%** from 2022 to 2024
- Clear seasonal peaks around **March** and **August–September**
- **ARIMA(0,1,1)** outperformed ETS on holdout accuracy (**7.5% MAPE vs 9.2% MAPE**)
- K-means clustering grouped crime categories into three distinct behavioral clusters
- "Miscellaneous" and "Other Theft" dominate total case volume, while violent crimes (Murder, Kidnapping) show steadier, more predictable seasonal patterns


## 📁 Repository Structure

```
CRIME-TREND-ANALYSIS-AND-FORECASTING-IN-PAKISTAN/
├── scripts/                          # R scripts, one per project phase
│   ├── 01_data_import.R
│   ├── 02_data_cleaning.R
│   ├── 03_data_transformation.R
│   ├── 04_eda.R
│   ├── 05_statistical_analysis.R
│   ├── 06_regression_analysis.R
│   ├── 07_clustering.R
│   ├── 08_time_series_analysis.R
│   ├── 09_forecasting.R
│   └── 10_report.Rmd                 # R Markdown source for the final report
├── figures/                          # 20+ exported charts (EDA, regression, clustering, forecasts)
├── data/                             # raw and cleaned datasets (CSV)
├── CRIME TREND ANALYSIS AND FORECASTING IN PAKISTAN.pdf   # full written report
├── Moth-wise-Crime-data.xlsx         # original raw data (Ministry of Interior)
└── README.md
```
