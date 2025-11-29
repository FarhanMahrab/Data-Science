data("mtcars")
head(mtcars)



library(ggplot2)

ggplot(mtcars, aes(x = hp, y = mpg)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "HP vs MPG", x = "Horsepower", y = "Miles per Gallon")


ggplot(mtcars, aes(y = mpg)) +
  geom_boxplot(fill = "tomato") +
  labs(title = "Boxplot of Miles per Gallon", y = "MPG")


library(GGally)
ggcorr(mtcars, label = TRUE)


colSums(is.na(mtcars))


library(dplyr)

mtcars_clean <- na.omit(mtcars)
cat("Total NA values after cleaning:", sum(is.na(mtcars_clean)), "\n")

mtcars_clean <- mtcars_clean[!duplicated(mtcars_clean), ]


mtcars_filtered <- mtcars_clean %>% filter(mpg > 20)

mtcars_selected <- mtcars_filtered %>% select(mpg, hp, wt)

mtcars_mutated <- mtcars_selected %>%
  mutate(power_to_weight = hp / wt)


mtcars_scaled <- mtcars_selected %>%
  mutate(across(c(mpg, hp, wt), ~ scale(.)[,1]))
head(mtcars_scaled)