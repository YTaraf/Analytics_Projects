# Fire Damage Prediction Analysis

This project analyzes the relationship between the distance from the nearest fire station and the amount of damage caused by residential fires. It employs data visualization, correlation analysis, simple linear modeling, and diagnostic checks to understand and improve model performance.

---

## Overview
The goal of this analysis is to model the amount of damage (in thousands of dollars) in major residential fires based on the distance (in miles) between the burning house and the nearest fire station. The approach includes exploratory data analysis, simple linear regression, model diagnostics, and outlier handling.

---

## Data
The dataset (`Fires.csv`) contains:
- **DAMAGE**: Fire damage in thousands of dollars.
- **DISTANCE**: Distance in miles from the nearest fire station.

---

## Analysis Workflow

1. **Exploratory Data Analysis (EDA)**:
   - Displayed summary statistics and checked for missing values.
   - Sorted data by `DISTANCE` and `DAMAGE`.
   - Visualized data distributions using histograms.
   - Generated a correlation matrix.

2. **Model Building**:
   - Fitted a simple linear regression model: `DAMAGE ~ DISTANCE`.
   - Visualized the regression line on a scatter plot.

3. **Model Diagnostics**:
   - Checked residual plots for equal variance, independence, and normality.
   - Identified influential points using Cook's Distance.
   - Removed the most influential observation (row 13) and refitted the model.
   - Compared diagnostics before and after removing the influential point.

4. **Model Comparison**:
   - Plotted both regression lines (original and reduced models) to compare fit and influence of the outlier.

---

## Key Features
- Simple linear regression analysis of fire damage vs distance.
- Exploratory analysis including summary statistics, correlations, and histograms.
- Model diagnostics with residual plots and Cook's Distance.
- Outlier detection and impact analysis.
- Visual comparison of models before and after outlier removal.

---
