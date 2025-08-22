# Set working directory (modify as needed)
setwd("C:/Users/zahid/Downloads/USF/Data Science Courses/ISM 6137 Advanced Statistical Modeling/Group Project (School Shootings)")

# Load required library
library(readxl)

# Read the specified sheet into a dataframe
df <- read_excel("School Shootings.xlsx", sheet = "Shooter")

# Function to calculate missing values and their percentage
missing_info <- function(column) {
    missing_count <- sum(is.na(column))
    total_count <- length(column)
    missing_percentage <- (missing_count / total_count) * 100
    return(list(class = class(column)[1], missing_count = missing_count, missing_percentage = round(missing_percentage, 2)))
}

# Apply the function to each column and store results
result <- lapply(df, missing_info)

# Retrieve column names, data types, and missing value information
column_info <- data.frame(
    Column = names(df),
    DataType = sapply(result, function(x) x$class),
    MissingCount = sapply(result, function(x) x$missing_count),
    MissingPercentage = sapply(result, function(x) x$missing_percentage)
)

# Print the column information
print(column_info)
