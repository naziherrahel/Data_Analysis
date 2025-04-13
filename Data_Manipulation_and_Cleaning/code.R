#--------------------start-------------------------------
# Get current working directory
getwd()
# create path relative to project root
#data_path <- "/cloud/project/data/DataSet_No_Details.csv"
data_path <- "DataSet_No_Details.csv"
#----------------read dataset--------------------------
df <- read.csv(data_path)
# Display structure with variable types
str(df)
# Beautiful summary with histograms for numeric variables
install.packages("skimr")
library(skimr)
skim(df) 
#---------------data set preparation------------------
library(dplyr)
# Delete a few columns 
cols_to_remove <- c("h_index_34", "h_index_56", "hormone10_1", "hormone10_2","an_index_23","outcome","factor_eth","factor_h","factor_pcos","factor_prl")
MD_df <- df %>% select(-any_of(cols_to_remove))
factor_df <- df %>% select (record_id, outcome, factor_eth, factor_h,factor_pcos,factor_prl)
str(MD_df)
summary(factor_df)

#--------------Identify Missing Values-----------------
sum(is.na(MD_df))               # Total NAs in entire dataset
colSums(is.na(MD_df))           # NA counts per column
skim(MD_df)
na_stats <- colMeans(is.na(MD_df)) * 100 # % missing data
na_stats
na_stats_filtered <- na_stats[na_stats <= 35] #  missing data <=35 %
# result as a table
data.frame(
  Column = names(na_stats_filtered),
  NA_Percent = na_stats_filtered,
  row.names = NULL
)

na_stats_filtered_1 <- na_stats[na_stats > 35] # missing data >35 %
# result as a table
data.frame(
  Column = names(na_stats_filtered_1),
  NA_Percent = na_stats_filtered_1,
  row.names = NULL
)

#-------------------Visualizing Missing Data Patterns------------------
install.packages("visdat")
library(visdat)
vis_miss(MD_df)  # Visualizes NA patterns

install.packages('naniar')
library(naniar)
gg_miss_var(MD_df)  # Barplot of missingness per variable
#------------------ Analyzing the Impact of Missing Data--------------
# Delete a few columns 
library(dplyr)
cols_to_remove1 <- c("hormone9", "hormone11", "hormone12", "hormone13","hormone14")
handle_MD_df <- MD_df %>% select(-any_of(cols_to_remove1))
str(handle_MD_df)

#------------------Performing Little's MCAR Test----------------------
# Homework!!!
# Hypotheses:
#  H₀ (Null Hypothesis): Data is MCAR.
library(naniar)
handle_MD_df_numeric <- handle_MD_df %>% select(-record_id)
mcar_test_result <- mcar_test(handle_MD_df_numeric)
print(mcar_test_result)

# H₁ (Alternative Hypothesis): Data is not MCAR (either MAR or MNAR).

if (mcar_test_result$p.value > 0.05) {
  cat("p > 0.05，Fail to reject H₀ (data is MCAR)")
} else {
  cat("p ≤ 0.05，Reject H₀ (data is not MCAR, likely MAR/MNAR)")
}

# If p-value > 0.05, we fail to reject H₀ (data is likely MCAR).
# If p-value ≤ 0.05, we reject H₀ (data is likely not MCAR).
#------------------Imputation with MICE-------------------------------
# Install packages if they are not already installed
# install.packages(c("mice", "ggplot2", "naniar"))

# Load the packages
library(mice)
library(ggplot2)
library(naniar)

# Perform Multiple Imputation using PMM (Predictive Mean Matching)
imputed_handle_MD_df <- mice(handle_MD_df, m=5, method='pmm', print=FALSE)

# Perform Multiple Imputation using Random Forests (RF)
install.packages("ranger")
library(ranger)
imputed_handle_MD_df1 <- mice(handle_MD_df, m=5, method='rf', print=FALSE)

# Random Forest method for complex nonlinear relationships between variables
imputed_handle_MD_df <- mice(handle_MD_df[, !names(handle_MD_df) %in% "New"], method="pmm")  
imputed_handle_MD_df_final <- complete(imputed_handle_MD_df)  # Generate full data

# Density plots for hormone10_generated variable
ggplot(handle_MD_df, aes(x=hormone10_generated, fill="Original")) +
  geom_density(alpha=0.5) +
  geom_density(data=imputed_handle_MD_df_final, aes(x=hormone10_generated, fill="Imputed"), alpha=0.5) +
  labs(title="Density Plot of hormone10_generated: Original vs. Imputed") +
  scale_x_continuous(limits = c(0, 2)) +
  theme_minimal()

# Predictive Mean Matching
# Homework!!!
imputed_handle_MD_df1 <- mice(handle_MD_df[, !names(handle_MD_df) %in% "New"], method="rf")  
imputed_handle_MD_df_final1 <- complete(imputed_handle_MD_df1)  # Generate full data

# Density plots for hormone10_generated variable (after RF imputation)
ggplot(handle_MD_df, aes(x=hormone10_generated, fill="Original")) +
  geom_density(alpha=0.5) +
  geom_density(data=imputed_handle_MD_df_final1, aes(x=hormone10_generated, fill="Imputed"), alpha=0.5) +
  labs(title="Density Plot of hormone10_generated: Original vs. Imputed") +
  scale_x_continuous(limits = c(0, 2)) +
  theme_minimal()

#-------------------------export------------------------------
write.csv(imputed_handle_MD_df_final, "imputed_data.csv", row.names = FALSE)

#----------------Outlier Detection Methods------------------------
library(tidyr)
library(dplyr)

# Selecting variables for outlier detection
outliers_data <- imputed_handle_MD_df_final %>%
  select(lipids1, lipids2, lipids3, lipids4, lipids5) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value")

# Create a boxplot for outlier detection
ggplot(outliers_data, aes(x = variable, y = value)) +
  geom_boxplot(fill = "lightblue", alpha = 0.7) +
  labs(title = "Outlier Detection",
       x = "Variables",
       y = "Value") +
  theme_minimal()

# Boxplot for all numeric data for outlier detection
imputed_handle_MD_df_final %>%
  select(where(is.numeric)) %>%
  pivot_longer(everything()) %>%
  ggplot(aes(y = value)) +
  geom_boxplot() +
  facet_wrap(~name, scales = "free") +
  labs(title = "Boxplots for Outlier Detection") +
  theme_minimal()
