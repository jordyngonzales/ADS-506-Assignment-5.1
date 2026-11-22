# Australian Wine Sales Analysis

**Live App:** https://01a2051f-912a-bee4-2950-1f0cada50904.share.connect.posit.cloud

## Overview

Interactive Shiny application for comparing forecasting models on monthly Australian wine sales by varietal. Users choose varietals, a date range and where the training window ends; the app fits TSLM, ETS and ARIMA models, reports their specifications and accuracy, and plots forecasts with prediction intervals.

## Features

- **Overview, Seasonality and Decomposition**: time plots, seasonal plots and an STL decomposition of the filtered data
- **Model specs & accuracy**: readable model specifications (e.g., ETS(A,N,A), ARIMA(p,d,q)(P,D,Q)[12]), training vs validation RMSE, MAE and MAPE, a best-model summary and a CSV download
- **Validation forecasts**: models fit through a user-chosen training-end month and forecast over the held-out months, with prediction intervals
- **Future forecasts**: models re-fit on all selected data and forecast a user-defined horizon (1–36 months)
- **Accuracy options**: optional log transform and an equal-weight ensemble of the selected models
- **About tab**: data source, modeling choices and steps to reproduce each figure

## Data

Australian wine sales data by varietal and month, measuring sales in thousands of liters.

## Models

- TSLM (Time Series Linear Model)
- ETS (Error, Trend, Seasonal)
- ARIMA (AutoRegressive Integrated Moving Average)
- Equal-weight ensemble of the selected models

## Usage
- Pick one or more varietals and a date range
- Set the training-end month. Months after it form the validation window
- Choose the models, and turn the log transform and ensemble on or off
- Set the future forecast horizon and the prediction interval (80% or 95%)
- Explore the tabs: data views, specs and accuracy, validation forecasts and future forecasts

## Dependencies

Built with R using `shiny`, `fpp3`, and `tidyverse`
