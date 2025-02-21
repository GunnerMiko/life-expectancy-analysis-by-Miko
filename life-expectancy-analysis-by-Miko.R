# Life expectancy analysis by Miko, based on Life Expectancy dataset from www.kaggle.com.

# Install Required Packages
install.packages("tibble")
install.packages("readr")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("patchwork")
install.packages("tidymodels")
install.packages("yardstick")
install.packages("parsnip")
install.packages("tune")
install.packages("workflows")
install.packages("recipes")
install.packages("ranger")
install.packages("xgboost")


# Load Libraries
library(tibble)
library(readr)
library(tidyverse)
library(dplyr)
library(patchwork)
library(tidymodels)
library(yardstick)
library(parsnip)
library(tune)
library(workflows)
library(recipes)
library(ranger)
library(xgboost)

######################################## Part A of the project - Data Loading, Cleaning and Exploratory Data Analysis. What features are impacting the life expectancy? Exploring the relationhips.###############################
##################################################################################################################################################################################################################################

# Load data from GitHub repository
data_url <- "https://raw.githubusercontent.com/GunnerMiko/life-expectancy-analysis-by-Miko/main/Life%20Expectancy%20Data.csv"

# Read the CSV File into a Tibble
life_data <- read_csv(data_url)

# Load the data into a tibble
life_data <- as_tibble(life_data)

# Check if data is dislayed using first few rows
head(life_data)

# Verify if there are missing values in the data
missing_summary <- life_data %>%
  summarize_all(~ sum(is.na(.)))
print(missing_summary)

# It looks like our target value "Life Expectancy" has some missing values. As there are only 10 of these records, I am removing them from the dataset to avoid any bias in the analysis.
life_data <- life_data %>%
  filter(!is.na(`Life expectancy`) & `Life expectancy` != "")

# Verify missing values again
missing_values_summary2 <- life_data %>%
  summarize_all(~ sum(is.na(.)))
print(missing_values_summary2)

# summarize and filter columns with missing and blank values, the missing values will be handeled later
missing_Values_final_summary <- life_data %>%
  summarize_all(~ sum(is.na(.))) %>%
  gather() %>%
  filter(value > 0)
print(missing_Values_final_summary)

# Let's check the data types of the columns
str(life_data)

# At this step I am identifying which features of the data set are numerical and which are categorical
numerical_features <- names(life_data)[sapply(life_data, is.numeric)]
categorical_features <- names(life_data)[sapply(life_data, is.character)]

# Print grouped features
cat("Numerical Features:\n")
print(numerical_features)
cat("\nCategorical Features:\n")
print(categorical_features)

#previously identified number of missing values in each column was quite meaningful so we will use imputing method to fill the missing values
print(missing_Values_final_summary)

# List of columns to replace missing values with mean
columns_to_replace <- c("Alcohol", "Hepatitis B", "BMI", "Polio", "Total expenditure",
                        "Diphtheria", "GDP", "Population", "thinness 1-19 years",
                        "thinness 5-9 years", "Income composition of resources", "Schooling")

# Replace missing values with mean
life_data_clean <- life_data %>%
  mutate(across(all_of(columns_to_replace), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Count missing values in each column for verification
missing_values_count <- colSums(is.na(life_data_clean))

# Print the count of missing values
print(missing_values_count)


#checking for outliesr using boxplot statistcs and removing them


# Define outliers
identify_outliers <- function(data, feature) {
  Q1 <- quantile(data[[feature]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data[[feature]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  return(data %>% filter(data[[feature]] < lower_bound | data[[feature]] > upper_bound))
}

# Remove outliers function
remove_outliers <- function(data, features) {
  for (feature in features) {
    Q1 <- quantile(data[[feature]], 0.25, na.rm = TRUE)
    Q3 <- quantile(data[[feature]], 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
    data <- data %>% filter(data[[feature]] >= lower_bound & data[[feature]] <= upper_bound)
  }
  return(data)
}

# List of numerical features to remove outliers from
numerical_vars <- c('Year', 'Life expectancy', 'Adult Mortality', 'infant deaths', 
                    'Alcohol', 'percentage expenditure', 'Hepatitis B', 'Measles', 
                    'BMI', 'under-five deaths', 'Polio', 'Total expenditure', 
                    'Diphtheria', 'HIV/AIDS', 'GDP', 'Population', 
                    'thinness 1-19 years', 'thinness 5-9 years', 
                    'Income composition of resources', 'Schooling')

# Remove outliers from the dataset
life_data_clean_outliers <- remove_outliers(life_data_clean, numerical_vars)

# Combine the original and cleaned data for comparison
life_data_long_before <- life_data_clean %>%
  select(numerical_vars) %>%
  gather(key = "feature", value = "value") %>%
  mutate(Type = "Before")

life_data_long_after <- life_data_clean_outliers %>%
  select(numerical_vars) %>%
  gather(key = "feature", value = "value") %>%
  mutate(Type = "After")

comparison_data <- bind_rows(life_data_long_before, life_data_long_after)

# Create comparison boxplots for all numerical features
ggplot(comparison_data, aes(x = feature, y = value, fill = Type)) +
  geom_boxplot() +
  labs(title = "Comparison BoxPlots Before and After Removing Outliers",
       x = "Feature",
       y = "Value") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Create comparison box plot for BMI before and after outlier removal
ggplot(comparison_data %>% filter(feature == "BMI"), aes(x = Type, y = value, fill = Type)) +
  geom_boxplot() +
  labs(title = "Comparison BoxPlot for BMI Before and After Outlier Removal", y = "BMI", x = "") +
  theme_minimal()

# Create comparison box plot for Population before and after outlier removal
ggplot(comparison_data %>% filter(feature == "Population"), aes(x = Type, y = value, fill = Type)) +
  geom_boxplot() +
  labs(title = "Comparison BoxPlot for Population Before and After Outlier Removal", y = "Population", x = "") +
  theme_minimal()

######################################### Exploratory Data Analysis - EDA #########################################################################################################################################################################
##################################################################################################################################################################################################################################
#In the previous steps we defined the numerical and categorical features of the dataset. Now we will explore the relationships between these features and the target variable "Life Expectancy".

# List of numerical and categorical variables based on the dataset
numerical_vars <- c('Year', 'Life expectancy', 'Adult Mortality', 'infant deaths', 
                    'Alcohol', 'percentage expenditure', 'Hepatitis B', 'Measles', 
                    'BMI', 'under-five deaths','Polio', 'Total expenditure', 
                    'Diphtheria', 'HIV/AIDS', 'GDP', 'Population', 
                    'thinness 1-19 years', 'thinness 5-9 years', 
                    'Income composition of resources', 'Schooling')

categorical_vars <- c('Country', 'Status')

# Create a correlation matrix
correlation_matrix <- life_data_clean_outliers %>%
  select(numerical_vars) %>%
  cor(use = "complete.obs")

# Convert correlation matrix to a data frame
correlation_matrix_df <- as.data.frame(as.table(correlation_matrix))

# Renaming the columns for better readability
colnames(correlation_matrix_df) <- c("Variable1", "Variable2", "Correlation")


# Create a correlation plot
ggplot(data = correlation_matrix_df, aes(x = Variable1, y = Variable2, fill = Correlation)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab", name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        legend.justification = c(1, 0),
        legend.position = "bottom",
        legend.direction = "horizontal") +
  coord_fixed(ratio = 1)  

##Based on the correlation matrix, we can see that the features "Schooling", "Income composition of resources", "BMI", "Diphtheria", "Polio" and "GDP" have a high positive correlation with the target variable "Life expectancy".
##On the other hand, the features "Adult Mortality", "HIV/AIDS", "thinness 1-19 years" and "thinness 5-9 years" have a high negative correlation with the target variable "Life expectancy".


######################################## Part 1B of the project: Building a Prediction Model with linear regression ########################################
##################################################################################################################################

# Preparing the Data for Modeling

# Convert categorical variables to factors
life_data_model <- life_data_clean_outliers %>%
  mutate(Status = as.factor(Status),
         Country = as.factor(Country))

# Handle infinite values and drop NA values safely
life_data_model <- life_data_model %>%
  mutate(across(where(is.numeric), ~ ifelse(is.infinite(.), NA, .))) %>%
  drop_na()

# Check dataset dimensions
cat("Train data dimensions:", dim(life_data_model), "\n")

# Define Preprocessing Recipe
life_recipe <- recipe(`Life expectancy` ~ ., data = life_data_model) %>%
  step_impute_mean(all_numeric_predictors()) %>%  # Fill missing values with mean
  step_zv(all_numeric_predictors()) %>%  # Remove zero-variance predictors
  step_mutate(across(where(is.numeric), ~ ifelse(is.infinite(.), NA, .))) %>%  # Handle Inf values
  step_normalize(all_numeric_predictors()) %>%  # Normalize numerical features
  step_dummy(all_nominal_predictors())  # Encode categorical variables


###########Model Training & Evaluation########
##############################################

# Split Data into Train & Test Sets
set.seed(123)
data_split <- initial_split(life_data_model, prop = 0.8)
train_data <- training(data_split)
test_data <- testing(data_split)

# Define Linear Regression Model
lin_reg_spec <- linear_reg() %>%
  set_engine("lm")

# Create Workflow
life_workflow <- workflow() %>%
  add_model(lin_reg_spec) %>%
  add_recipe(life_recipe)

# Train the Model
life_model <- life_workflow %>%
  fit(data = train_data)

# Make Predictions on Test Data
test_predictions <- life_model %>%
  predict(test_data) %>%
  bind_cols(test_data)

# Compute Model Performance Metrics
metrics_results <- test_predictions %>%
  metrics(truth = `Life expectancy`, estimate = .pred)

# Print Model Performance
print(metrics_results)


############################## Visualization #########################
######################################################################

#Scatter Plot: Actual vs. Predicted
ggplot(test_predictions, aes(x = `Life expectancy`, y = .pred)) +
  geom_point(color = "blue", alpha = 0.5) +  
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +  
  labs(title = "Actual vs. Predicted Life Expectancy",
       x = "Actual Life Expectancy",
       y = "Predicted Life Expectancy") +
  theme_minimal()

#Residual Plot
ggplot(test_predictions, aes(x = .pred, y = `Life expectancy` - .pred)) +
  geom_point(alpha = 0.5, color = "purple") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residual Plot",
       x = "Predicted Life Expectancy",
       y = "Residuals (Actual - Predicted)") +
  theme_minimal()

#Histogram of Prediction Errors
ggplot(test_predictions, aes(x = `Life expectancy` - .pred)) +
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Prediction Errors",
       x = "Prediction Error (Residuals)",
       y = "Count") +
  theme_minimal()

# While using Linear regression modelling, which assumes a straight-line relationship, I obtained following final results: 


# 1 rmse    standard       1.46  - model's predictions for life expectancy deviate by 1.46 years from the actual life expectancy values
# 2 rsq     standard       0.880 - 88.0% of the variability in life expectancy is explained was explained by model
# 3 mae     standard       0.873 - model's predictions for life expectancy deviate by 0.87 years from the actual life expectancy values




