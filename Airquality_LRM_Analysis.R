
library(ggplot2)
library(dplyr)
library(caret)
library(Metrics)


data("airquality")

summary(airquality)

missing_values <- colSums(is.na(airquality))
print(paste("Missing values per column:", missing_values))

airquality_clean <- airquality %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# 1. Exploratory Data Analysis (EDA) -

ggplot(airquality_clean, aes(x = Wind, y = Ozone)) + 
  geom_point() + labs(title = "Scatterplot of Wind vs Ozone")

ggplot(airquality_clean, aes(x = Ozone)) + 
  geom_histogram(binwidth = 10, fill = "blue", color = "black") + labs(title = "Distribution of Ozone Levels", x = "Ozone", y = "Frequency")

ggplot(airquality_clean, aes(x = Solar.R, y = Ozone)) + 
  geom_point() + labs(title = "Scatterplot of Solar Radiation vs Ozone")

correlation_matrix <- cor(airquality_clean[, sapply(airquality_clean, is.numeric)])
print("Correlation Matrix:")
print(correlation_matrix)

# 2. Model Building - 

model <- lm(Ozone ~ ., data = airquality_clean)
summary(model)

# 3. Model Evaluation -

par(mfrow = c(2, 2))

plot(model)

# 4. Performance Metrics -

predictions <- predict(model, airquality_clean)


rmse_value <- rmse(airquality_clean$Ozone, predictions)
print(paste("RMSE: ", rmse_value))

# 5. Model Selection - 

print("Model Coefficients:")
print(coef(model))


