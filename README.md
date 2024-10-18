# Proactive-Engagement-Analytics-Predicting-Customer-Retention-Interaction-Trends
Predicting customer engagement for SaaS businesses to reduce churn.


# SaaS Customer Engagement Forecasting Project

## Overview:
This project focuses on predicting periods of low engagement in a SaaS business by forecasting **logins per week** and **session duration per week**. The goal is to help customer success teams proactively engage customers and reduce churn.

## Problem Statement:
The SaaS industry heavily relies on recurring subscriptions, but customer churn poses a significant threat. By predicting low engagement periods, the customer success team can intervene early and retain customers.

## Dataset:
We used a dataset of 520,000 observations spanning 12 months, with the following features:
- **LoginsPerWeek**
- **SessionDurationPerWeek**
- **FeatureInteractionsPerWeek**
- **TimeSinceLastLogin**
- **LifecycleStage**

## Tools and Techniques:
- **Programming Language**: R
- **Model**: ARIMA for time series forecasting
- **Evaluation Metrics**: MAE, RMSE, Shapiro-Wilk, Ljung-Box
- **Visualization**: Trends, forecasts, and comparisons between predicted and actual values

## Key Findings:
The ARIMA model provided accurate forecasts, with low MAE and RMSE values, indicating the model captured engagement trends effectively. This provides actionable insights for the customer success team.

## Files:
- `data/`: Contains raw and cleaned datasets.
- `scripts/`: Includes all R scripts for data preparation, modeling, and evaluation.
- `results/`: Forecasted results and plots.

## Conclusion:
This project demonstrates my ability to work with time series data, build predictive models, and generate actionable insights to improve customer retention in a SaaS business.
