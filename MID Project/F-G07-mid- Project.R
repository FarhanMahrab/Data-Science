#List of required packages
packages <- c("rvest", "stringr","dplyr","corrplot","randomForest","readr","caret","googledrive","GGally","e1071","ggplot2","githubr")

# Install any packages that are not already installed
installed_packages <- rownames(installed.packages())
for (pkg in packages) {
  if (!(pkg %in% installed_packages)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}









                     #A. Data Understanding

# 1.Data Load into R
data <- read.csv("https://raw.githubusercontent.com/FarhanMahrab/Heart-Disease/refs/heads/main/heart_disease.csv")


# 2.Display the first few rows of the dataset.
head(data)



# 3. Show shape (rows × columns).
cat("Rows:", nrow(data), "\n")
cat("Columns:", ncol(data), "\n")


# 4. Display data types of each column.
str(data)


# 5. Generate basic descriptive statistics
# Mean, Median, Std, Min, Max
summary_stats <- data.frame(
  Mean = sapply(data, function(x) if(is.numeric(x)) mean(x, na.rm = TRUE) else NA),
  Median = sapply(data, function(x) if(is.numeric(x)) median(x, na.rm = TRUE) else NA),
  StdDev = sapply(data, function(x) if(is.numeric(x)) sd(x, na.rm = TRUE) else NA),
  Min = sapply(data, function(x) if(is.numeric(x)) min(x, na.rm = TRUE) else NA),
  Max = sapply(data, function(x) if(is.numeric(x)) max(x, na.rm = TRUE) else NA),
  Count = sapply(data, function(x) sum(!is.na(x)))
)

summary_stats

# MODE function (because R doesn't have one)
get_mode <- function(v) {
  uniq_vals <- unique(v)
  uniq_vals[which.max(tabulate(match(v, uniq_vals)))]
}

mode_values <- sapply(data, function(x) {
  if(is.numeric(x) || is.character(x)) get_mode(x) else NA
})

mode_values



# 6. Identify categorical & numerical features

numeric_features <- names(data)[sapply(data, is.numeric)]
categorical_features <- names(data)[sapply(data, is.factor) | sapply(data, is.character)]

cat("Numeric Columns:\n")
print(numeric_features)

cat("\nCategorical Columns:\n")
print(categorical_features)










               # B. Data Exploration & Visualization

# 1. Univariate Analysis

`#Histogram for all numeric columns


numeric_cols <- names(data)[sapply(data, is.numeric)]

for(col in numeric_cols){
  hist(data[[col]], 
       main = paste("Histogram of", col),
       xlab = col,
       col = "skyblue")
}





# Boxplot for numeric columns

for(col in numeric_cols){
  boxplot(data[[col]], 
          main = paste("Boxplot of", col),
          col = "orange",
          horizontal = TRUE)
}



# BAR CHART for Categorical Variables

# Identify categorical columns
cat_cols <- names(data)[sapply(data, is.character) | sapply(data, is.factor)]

# Create bar charts for each categorical variable
for(col in cat_cols){
  barplot(table(data[[col]]),
          main = paste("Bar Chart of", col),
          xlab = col,
          ylab = "Frequency",
          col = "lightblue",
          las = 2)   # makes labels readable
}






# Frequency of categorical variables
cat_cols <- names(data)[sapply(data, is.character) | sapply(data, is.factor)]

for(col in cat_cols){
  barplot(table(data[[col]]),
          main = paste("Frequency of", col),
          col = "lightgreen",
          las = 2)
}





                      # 2. Bivariate Analysis


# Correlation Heatmap
# use = "complete.obs" ignores missing values so cor() doesn't return NA
corr_matrix <- cor(data[, numeric_cols], use = "complete.obs") 


corrplot(corr_matrix, 
         method = "color", 
         type = "upper",        # Optional: shows only upper triangle
         addCoef.col = "black", # Optional: adds number labels
         tl.col = "black",      # Optional: makes text labels black
         tl.cex = 0.8)          # Optional: adjusts text size









# Scatter plots between numeric variable pairs
# Define the target column for coloring (e.g., Heart Disease Status)
target_col_name <- tail(names(data), 1)

# Loop through all numeric columns and plot them against Age
for(col in numeric_cols){
  if(col != "Age"){ # Skip plotting Age against itself
    
    p <- ggplot(data, aes_string(x = "Age", y = col, color = target_col_name)) +
      geom_point(alpha = 0.6) +
      labs(title = paste("Scatter Plot: Age vs.", col),
           x = "Age",
           y = col) +
      theme_minimal()
    
    print(p) # Display the plot
  }
}












# Boxplots between categorical & numeric
if(length(cat_cols) > 0){
  for(cat in cat_cols){
    for(num in numeric_cols){
      boxplot(data[[num]] ~ data[[cat]], 
              main = paste(num, "vs", cat),
              xlab = cat,
              ylab = num,
              col = "pink")
    }
  }
}








# 3. Identify Patterns, Skewness & Outliers


#  patterns

## Assume the LAST column is the target (change if your teacher told a specific one)
target_col <- tail(names(data), 1)
cat("Using target column for pattern analysis:", target_col, "\n")



#  Correlation of numeric features with the target
cor_with_target <- cor(data[, numeric_cols], as.numeric(as.factor(data[, target_col])), use = "complete.obs")
cor_with_target



#  Mean values of numeric features grouped by target
group_means <- data %>% group_by(.data[[target_col]]) %>%
  summarise(across(all_of(numeric_cols), mean, na.rm = TRUE))
group_means



# Frequency comparison for each categorical variable
for(col in cat_cols){
  print(table(data[[col]], data[[target_col]]))
}






# Skewness (Skew > 1 = right skew , Skew < -1 = left skew , In between = approximately normal)
skew_values <- sapply(data[, numeric_cols], skewness)
skew_values






# Outlier detection using Boxplot stats (IQR method)
outliers_list <- list()

for(col in numeric_cols){
  stats <- boxplot.stats(data[[col]])
  outliers_list[[col]] <- stats$out
}

outliers_list












                    # C. Data Preprocessing

# 1. Handling Missing Values

# Count missing values per column
colSums(is.na(data))

# Total missing values
sum(is.na(data))




# Function to compute mode
get_mode <- function(v) {
  uniq <- unique(v)
  uniq[which.max(tabulate(match(v, uniq)))]
}

# Replace missing values
for(col in names(data)){
  if(is.numeric(data[[col]])){
    data[[col]][is.na(data[[col]])] <- mean(data[[col]], na.rm = TRUE)
  } else {
    data[[col]][is.na(data[[col]])] <- get_mode(data[[col]])
  }
}







# 2. Handling Outliers


numeric_cols <- names(data)[sapply(data, is.numeric)]
outliers <- list()

for(col in numeric_cols){
  Q1 <- quantile(data[[col]], 0.25)
  Q3 <- quantile(data[[col]], 0.75)
  IQR_val <- IQR(data[[col]])
  
  lower <- Q1 - 1.5 * IQR_val
  upper <- Q3 + 1.5 * IQR_val
  
  outliers[[col]] <- data[[col]][data[[col]] < lower | data[[col]] > upper]
}
outliers

# Remove outliers


for(col in numeric_cols){
  Q1 <- quantile(data[[col]], 0.25)
  Q3 <- quantile(data[[col]], 0.75)
  IQR_val <- IQR(data[[col]])
  
  lower <- Q1 - 1.5 * IQR_val
  upper <- Q3 + 1.5 * IQR_val
  
  data <- data[data[[col]] >= lower & data[[col]] <= upper, ]
}







# 3. Data Conversion
# Convert categorical variables: Label Encoding
cat_cols <- names(data)[sapply(data, is.character) | sapply(data, is.factor)]

for(col in cat_cols){
  data[[col]] <- as.numeric(as.factor(data[[col]]))
}


#Ensure everything is numeric

str(data)



# 4. DATA TRANSFORMATION

#Standardization (Z-score scaling)


data_scaled <- as.data.frame(scale(data[, numeric_cols]))


# Min-Max Normalization (0 to 1)

normalize <- function(x){ (x - min(x)) / (max(x) - min(x)) }
data_norm <- as.data.frame(lapply(data[, numeric_cols], normalize))


#-----------------Log transform (fix skewness)------------------


data_log <- data
for(col in numeric_cols){
  data_log[[col]] <- log(data[[col]] + 1)
}






# 5. Feature Selection
# We already used this idea before:
# target_col = last column = Heart.Disease.Status
target_col <- tail(names(data), 1)

# Convert target to numeric (0/1) so we can calculate correlation
target_num <- as.numeric(as.factor(data[[target_col]]))




# 1) Correlation between each numeric feature and the target
cor_with_target <- sapply(numeric_cols, function(col){
  cor(data[[col]], target_num, use = "complete.obs")
})

# Sort by absolute correlation (strongest first)
cor_with_target <- sort(abs(cor_with_target), decreasing = TRUE)
cor_with_target   # you can show this to teacher

# 2) Select top 5 most important numeric features
selected_features <- names(cor_with_target)[1:5]
selected_features  # print selected feature names

# 3) Create final dataset with only selected features + target
data_selected <- data[, c(selected_features, target_col)]
str(data_selected)