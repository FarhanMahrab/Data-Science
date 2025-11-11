head(iris)


str(iris)


summary(iris)


mean(iris$Sepal.Length)

median(iris$Sepal.Length)


# To find the mode, create a table of frequencies
freq_table <- table(iris$Sepal.Length)
# Find the name of the most frequent value
names(freq_table)[which.max(freq_table)]



# Calculate the range
range_val <- range(iris$Sepal.Length)
max(range_val) - min(range_val)


# Calculate variance
var(iris$Sepal.Length)


# Calculate standard deviation
sd(iris$Sepal.Length)


# Calculate the Interquartile Range
IQR(iris$Sepal.Length)


# Get specific quantiles (e.g., 25th and 75th percentiles)
quantile(iris$Sepal.Length, probs = c(0.25, 0.75))







library(dplyr)

# Calculate mean, sd, and count for each species
iris %>%
  group_by(Species) %>%
  summarise(
    count = n(),
    mean_sepal_length = mean(Sepal.Length),
    sd_sepal_length = sd(Sepal.Length),
    mean_petal_length = mean(Petal.Length),
    sd_petal_length = sd(Petal.Length)
  )








pairs(iris[, 1:4], main = "Scatterplot Matrix of Iris Data", col = iris$Species)






library(readr)
url <- "https://drive.google.com/uc?id=1On4FJezWA5ECF5co2_L1lYPdGzKzpok2"
dataset <- read_csv(url)

head(dataset)


head(iris)

str(iris)

summary(iris)


mean(iris$Sepal.Length)

median(iris$Sepal.Length)


# To find the mode, create a table of frequencies
freq_table <- table(iris$Sepal.Length)
# Find the name of the most frequent value
names(freq_table)[which.max(freq_table)]



# Calculate the range
range_val <- range(iris$Sepal.Length)
max(range_val) - min(range_val)


# Calculate variance
var(iris$Sepal.Length)


# Calculate standard deviation
sd(iris$Sepal.Length)


# Calculate the Interquartile Range
IQR(iris$Sepal.Length)


# Get specific quantiles (e.g., 25th and 75th percentiles)
quantile(iris$Sepal.Length, probs = c(0.25, 0.75))
 