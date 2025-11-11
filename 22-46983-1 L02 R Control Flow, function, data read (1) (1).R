x <- 10
if (x > 5) {
  print("x is greater than 5")
}


x <- 3
if (x > 5) {
  print("x is greater than 5")
} else {
  print("x is 5 or less")
}


score <- 75
if (score >= 90) {
  print("Grade A")
} else if (score >= 80) {
  print("Grade B")
} else if (score >= 70) {
  print("Grade C")
} else {
  print("Grade F")
}


for (i in 1:5) {
  print(paste("Iteration", i))
}


#repeat Loop (with break)
i <- 1
repeat {
  print(i)
  i <- i + 1
  if (i > 5) break
}


#next Statement (skip to next iteration)
for (i in 1:5) {
  if (i == 3) next
  print(i)
}


#break Statement (exit the loop)
for (i in 1:5) {
  if (i == 4) break
  print(i)
}


#mean()
numbers <- c(10, 20, 30, 40, 50)
mean(numbers)  # Output: 30


sum(numbers)  # Output: 150


length(numbers)  # Output: 5


#round()
pi_val <- 3.14159
round(pi_val, 2)  # Output: 3.14


paste("Hello", "World")  # Output: "Hello World"


#Simple function to add two numbers
add_numbers <- function(a, b) {
  return(a + b)
}

add_numbers(5, 3)  # Output: 8


#Function to check if a number is even
is_even <- function(x) {
  if (x %% 2 == 0) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

is_even(4)  # Output: TRUE


#Function with default parameter
greet <- function(name = "User") {
  paste("Hello", name)
}

greet()         # Output: "Hello User"


greet("Abir")   # Output: "Hello Abir"


#Anonymous (Lambda) Function with sapply()
numbers <- 1:5
squared <- sapply(numbers, function(x) x^2)
print(squared)  # Output: 1 4 9 16 25


#Reading a CSV File
data <- read.csv("c:/users/student/Downloads/Titanic_Modified.csv")
head(data)  # View the first few rows


#Reading a Text File (tab-delimited)
data <- read.table("c:/users/student/Downloads/Titanic_Modified.csv", header = TRUE, sep = "\t")
head(data)


#Reading Data from a URL
url <- "https://raw.githubusercontent.com/uiuc-cse/data-fa14/gh-pages/data/iris.csv"
data <- read.csv(url)
head(data)



#Exercise 1


# Create a variable score with a numeric value between 0 and 100
score <- 85

# If-else statement to print grade
if (score >= 90) {
  print("Excellent")
} else if (score >= 75) {
  print("Good")
} else if (score >= 50) {
  print("Pass")
} else {
  print("Fail")
}


#Exercise 2


# Create a numeric vector numbers from 1 to 10
numbers <- 1:10

# Use a for loop to print the square of each number
for (num in numbers) {
  print(num^2)
}



#Exercise 3


# Initialize a variable count = 1
count <- 1

# Use a while loop to print all even numbers less than 20
while (count < 20) {
  if (count %% 2 == 0) {
    print(count)
  }
  count <- count + 1
}


#Exercise 4


# Create a function multiply that takes two numbers and returns their product
multiply <- function(a, b) {
  return(a * b)
}

# Test the function with two numbers
result <- multiply(5, 6)
print(result)


#Exercise 5


# Create a function calculate_stats that takes a numeric vector and returns a list with mean, median, and standard deviation
calculate_stats <- function(numbers) {
  stats <- list(
    mean = mean(numbers),
    median = median(numbers),
    sd = sd(numbers)
  )
  return(stats)
}

# Test it with a vector of 5 numbers
stats_result <- calculate_stats(c(2, 5, 8, 12, 15))
print(stats_result)


#Exercise 6


# Write a function grade_result that takes a numeric score and prints the grade
grade_result <- function(score) {
  if (score >= 90) {
    print("A")
  } else if (score >= 75) {
    print("B")
  } else if (score >= 50) {
    print("C")
  } else {
    print("F")
  }
}

# Test it with different scores
grade_result(95)  # A
grade_result(80)  # B
grade_result(60)  # C
grade_result(40)  # F


#Exercise 7


# Read the CSV file into R using read.csv()
students <- read.csv("c:/users/student/Downloads/Titanic_Modified.csv")

# Display the first 5 rows and the structure of the dataset
head(students, 5)
str(students)












