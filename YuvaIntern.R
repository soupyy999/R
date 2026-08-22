library(tidyverse)
library(ggplot2)
library(dplyr)
library(caret)
library(corrplot)

data <- read.csv("train.csv")

head(data)
dim(data)
str(data)
summary(data)
colSums(is.na(data))

median_age <- median(data$Age, na.rm = TRUE)
data$Age[is.na(data$Age)] <- median_age

mode_value <- names(sort(table(data$Embarked), decreasing = TRUE))[1]
data$Embarked[is.na(data$Embarked)] <- mode_value

data$CabinKnown <- ifelse(is.na(data$Cabin), "No", "Yes")
data$Cabin <- NULL

colSums(is.na(data))

boxplot(data$Fare,
        main = "Fare Outlier Check",
        ylab = "Fare")

Q1 <- quantile(data$Fare, 0.25)
Q3 <- quantile(data$Fare, 0.75)
IQR_value <- IQR(data$Fare)

lower <- Q1 - 1.5 * IQR_value
upper <- Q3 + 1.5 * IQR_value

outliers <- data$Fare[data$Fare < lower | data$Fare > upper]
length(outliers)

min_max <- function(x) {
  (x - min(x, na.rm = TRUE)) /
    (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

data$Age_scaled <- min_max(data$Age)
data$Fare_scaled <- min_max(data$Fare)

summary(data[, c("Age_scaled", "Fare_scaled")])

data$Sex_encoded <- ifelse(data$Sex == "male", 0, 1)

embarked_encoded <- model.matrix(~ Embarked - 1, data = data)

head(embarked_encoded)

data %>%
  summarise(
    Average_Age = mean(Age, na.rm = TRUE),
    Median_Age = median(Age, na.rm = TRUE),
    Average_Fare = mean(Fare, na.rm = TRUE),
    Median_Fare = median(Fare, na.rm = TRUE),
    Average_SibSp = mean(SibSp, na.rm = TRUE),
    Average_Parch = mean(Parch, na.rm = TRUE)
  )

ggplot(data, aes(x = factor(Survived))) +
  geom_bar() +
  labs(title = "Number of Passengers by Survival Status",
       x = "Survived (0 = No, 1 = Yes)",
       y = "Number of Passengers")

ggplot(data, aes(x = Sex, fill = factor(Survived))) +
  geom_bar(position = "dodge") +
  labs(title = "Survival by Sex",
       x = "Sex", fill = "Survived")

ggplot(data, aes(x = factor(Pclass), fill = factor(Survived))) +
  geom_bar(position = "dodge") +
  labs(title = "Survival by Passenger Class",
       x = "Passenger Class", fill = "Survived")

ggplot(data, aes(x = Age)) +
  geom_histogram(bins = 30) +
  labs(title = "Distribution of Passenger Age",
       x = "Age", y = "Count")

ggplot(data, aes(x = Fare)) +
  geom_histogram(bins = 30) +
  labs(title = "Distribution of Passenger Fare",
       x = "Fare", y = "Count")

numeric_data <- data[, c("Survived", "Pclass", "Age",
                         "SibSp", "Parch", "Fare")]

cor_matrix <- cor(numeric_data, use = "complete.obs")

print(cor_matrix)

corrplot(cor_matrix, method = "number", type = "upper")

write.csv(data, "titanic_cleaned.csv", row.names = FALSE)