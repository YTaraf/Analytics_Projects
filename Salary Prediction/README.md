# Salary Prediction Analysis

This project focuses on analyzing salary data using R. It includes data preprocessing, exploratory data analysis (EDA), multiple linear modeling, and model diagnostics to identify factors influencing salary levels.

## Overview
This project aims to explore and model the relationship between employee salary and several predictors such as experience, education, bonus eligibility, board membership, and more. The approach includes correlation analysis, visualization, model selection (forward, backward, stepwise), and diagnostics of linear regression models.

---

## Data
The dataset used (`Salaries.csv`) includes:
- **Ln_Salary**: Natural log of salary
- **Experience**: Years of experience
- **Education**: Education level
- **Bonus**: Bonus eligibility (factor)
- **Supervised**: Number supervised
- **Assets**: Asset management indicator
- **Board_Member**: Board membership indicator (factor)
- **Age**: Age of the individual
- **Profits**: Company profit measure
- **International**: International responsibility indicator (factor)
- **Sales**: Company sales

---

## Analysis Workflow

1. **Preprocessing**:  
   - Renamed columns for clarity.  
   - Converted categorical variables to factors.  
   - Counted occurrences of categories.

2. **Exploratory Data Analysis (EDA)**:  
   - Examined correlations among numeric variables using `ggcorrplot`.  
   - Created boxplots of `Ln_Salary` versus categorical variables.

3. **Model Building**:  
   - Built multiple linear regression models.  
   - Applied backward, forward, and stepwise selection techniques.  
   - Selected the best model.

4. **Model Evaluation**:  
   - Visualized model diagnostics: residuals vs fitted, Q-Q plot, scale-location, residuals vs leverage.  
   - Identified influential points with Cook's distance.  
   - Examined Variance Inflation Factor (VIF) to check multicollinearity.

5. **Visualization**:  
   - Generated pairwise scatterplots of selected predictors and actual salaries (non-log).

---

## Key Features
- Automated feature selection (stepwise regression).
- Comprehensive EDA with correlation and boxplots.
- Model diagnostics and identification of outliers.
- Multicollinearity assessment using VIF.
- Data transformation and back-transformation of salary for visualization.

---
