# Data Manipulation and Cleaning - task 1

## Assignment Overview
In this module, we perform data cleaning, imputation, and outlier detection on a given dataset. The task involves handling missing values, visualizing the distributions of the variables before and after imputation, and detecting outliers using boxplots.

## Data Description
- The dataset contains various biological and health-related data (e.g., lipids levels, hormones, etc.).
- Missing data handling and imputation are performed using the MICE (Multiple Imputation by Chained Equations) technique.

## R Version
- R version 4.4.3 was used for the analysis.

## Procedures
1. Little's MCAR test is performed to assess if the missing data is missing completely at random.
2. Multiple Imputation is carried out using Predictive Mean Matching (PMM) and Random Forest (RF) methods.
3. Outlier detection is conducted through boxplots.
4. The results and visualizations are saved for submission.

## Files:
- **DataSet_No_Details.csv**: Original data before manipulation.
- **code.R**: R script used for data cleaning, imputation, and analysis.
- **imputed_handle_MD_df_final.csv**: Data after imputation and cleaning.
- **graphs/**: Folder containing plots like density and boxplots.
