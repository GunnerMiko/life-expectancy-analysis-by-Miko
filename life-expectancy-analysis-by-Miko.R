# Install Required Packages
install.packages("tibble")
install.packages("readr")

# Load Libraries
library(tibble)
library(readr)

# Construct the URL to Your Dataset
url <- "https://raw.githubusercontent.com/GunnerMiko/life-expectancy-analysis-by-Miko/main/Life%20Expectancy%20Data.csv"

# Read the CSV File into a Tibble
data <- read_csv(url)

# View the data
print(data)