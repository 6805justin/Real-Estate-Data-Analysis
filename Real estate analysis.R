# Install and load necessary packages
packages <- c("dplyr", "readr", "ggplot2", "corrplot", "gridExtra", "scales", "tidyr")
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

library(dplyr)
library(readr)
library(ggplot2)
library(corrplot)
library(gridExtra)
library(scales)
library(tidyr)

# Load datasets
apartment_prices <- read_csv("C:/Users/jacob/Downloads/Data (1)/Data/Apartment_prices.csv")
historical_demographic <- read_csv("C:/Users/jacob/Downloads/Data (1)/Data/Historical_demographic.csv")
projected_demographic <- read_csv("C:/Users/jacob/Downloads/Data (1)/Data/Projected_demographic.csv")

# Merge datasets on 'Suburb'
merged_data <- apartment_prices %>%
  inner_join(historical_demographic, by = "Suburb_name") %>%
  inner_join(projected_demographic, by = "Suburb_name")

# Convert columns to numeric
columns_to_numeric <- c("Median_price_2023", "Historical_population_growth", 
                        "Historical_median_income", "Historical_unemployment_rate",
                        "Historical_priority_growth_area", "Projected_population_growth", 
                        "Projected_median_income", "Projected_unemployment_rate",
                        "Projected_priority_growth_area")

summary(merged_data)
merged_data <- merged_data %>%
  mutate(across(all_of(columns_to_numeric), as.numeric))



# Check for NA values
na_summary <- sapply(merged_data, function(x) sum(is.na(x)))
print(na_summary)

# Handle missing values
mean_income <- mean(merged_data$Historical_median_income, na.rm = TRUE)

#handling wrong values
merged_data <- merged_data %>%
  mutate(
    Median_price_2023 = ifelse(Suburb_name == "NORLANE", 309334, Median_price_2023),
    Historical_median_income = ifelse(is.na(Historical_median_income), mean_income, Historical_median_income)
  )
# Check for NA values
na_summary <- sapply(merged_data, function(x) sum(is.na(x)))
print(na_summary)

# Calculate outliers for Median_price_2023
stats <- boxplot.stats(merged_data$Median_price_2023)
outliers1 <- stats$out

# Print outliers
print("Outliers in Historical_median_income:")
print(outliers1)




#Fixing outliers using IQR method
Q1_price <- quantile(merged_data$Median_price_2023, 0.25)
Q3_price <- quantile(merged_data$Median_price_2023, 0.75)
IQR_price <- Q3_price - Q1_price



merged_data <- merged_data %>%
  filter(
           Median_price_2023 > (Q1_price - 1.5 * IQR_price) & Median_price_2023 < (Q3_price + 1.5 * IQR_price))



# Calculate outliers for Median_price_2023
stats <- boxplot.stats(merged_data$Median_price_2023)
outliers2 <- stats$out

# Print outliers
print("Outliers in Historical_median_income:")
print(outliers2)



# Data Visualization

# 1. Distribution of Apartment Prices
p1 <- ggplot(merged_data, aes(x = Median_price_2023)) +
  geom_histogram(bins = 30, fill = "yellow", color = "black") +
  geom_vline(aes(xintercept = mean(Median_price_2023)), color = "purple", linetype = "dashed", size = 1) +
  scale_x_continuous(labels = comma) +
  labs(title = "Distribution of Apartment Prices", x = "Median Price 2023", y = "Count")

# 2. Box Plot of Apartment Prices
p2 <- ggplot(merged_data, aes(y = Median_price_2023)) +
  geom_boxplot(fill = "lightblue") +
  scale_y_continuous(labels = comma) +
  labs(title = "Box Plot of Apartment Prices", y = "Median Price 2023")


# 3. Scatter Plots
p3 <- ggplot(merged_data, aes(x = Historical_population_growth, y = Median_price_2023)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  scale_y_continuous(labels = comma) +
  labs(title = "Price vs Population Growth", x = "Historical Population Growth", y = "Median Price 2023")


p4 <- ggplot(merged_data, aes(x = Historical_median_income, y = Median_price_2023)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  labs(title = "Price vs Median Income", x = "Historical Median Income", y = "Median Price 2023")


p5 <- ggplot(merged_data, aes(x = Historical_unemployment_rate, y = Median_price_2023)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  scale_y_continuous(labels = comma) +
  labs(title = "Price vs Unemployment Rate", x = "Historical Unemployment Rate", y = "Median Price 2023")

# Arrange plots
q=grid.arrange(p1, p2, p3, p4, p5, ncol = 2)
q



# Correlation matrix
numeric_data <- merged_data %>% select(-Suburb_name)
a=cor_matrix <- cor(numeric_data, use = "complete.obs")


# Visualize correlation matrix
corrplot(cor_matrix, method = "color", type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black", number.cex = 0.7,
         col = colorRampPalette(c("blue", "yellow", "red"))(200))



# Prepare data for modeling
model_data <- merged_data %>%
  select(Median_price_2023, Historical_population_growth, Historical_median_income, 
         Historical_unemployment_rate, Historical_priority_growth_area, Projected_population_growth,Projected_median_income, Projected_unemployment_rate, Projected_priority_growth_area)

# Build linear regression model
model <- lm( Median_price_2023 ~ Historical_population_growth  + Historical_unemployment_rate + Historical_priority_growth_area, data = model_data)

# Print model summary
summary(model)
# Assuming you've run the model, interpret the results
coef_summary <- summary(model)$coefficients
print(coef_summary)

# Build GLM model
glm_model <- glm(Median_price_2023 ~ Historical_population_growth  + Historical_unemployment_rate + Historical_priority_growth_area, 
                 data = model_data, 
                 family = gaussian())

# Print model summary
summary(glm_model)
# Calculate standardized coefficients
std_coef <- coef(model) * sapply(model_data[,-1], sd) / sd(model_data$Median_price_2023)
print(std_coef)



# Visualize model residuals
p6 <- ggplot(data = model, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Fitted Values", x = "Fitted Values", y = "Residuals")

p7 <- ggplot(data = model, aes(sample = .resid)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Normal Q-Q Plot", x = "Theoretical Quantiles", y = "Sample Quantiles")

z=grid.arrange(p6, p7, ncol = 2)
z

# Predict future prices
future_data <- merged_data %>%
  select(Suburb_name, Projected_population_growth, Projected_median_income, 
         Projected_unemployment_rate, Projected_priority_growth_area) %>%
  rename(Historical_population_growth = Projected_population_growth,
         Historical_median_income = Projected_median_income,
         Historical_unemployment_rate = Projected_unemployment_rate,
         Historical_priority_growth_area = Projected_priority_growth_area)

future_prices <- predict(model, newdata = future_data)


# Combine predictions with suburb names
predictions <- data.frame(Suburb = future_data$Suburb_name, Predicted_Price = future_prices) %>%
  arrange(desc(Predicted_Price))

# Display top 10 highest median price suburbs
print(head(predictions, 10))

# Visualize top 10 predicted prices
ggplot(head(predictions, 10), aes(x = reorder(Suburb, Predicted_Price), y = Predicted_Price)) +
  geom_bar(stat = "identity", fill = "red") +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(title = "Top 10 Suburbs by Predicted Price", x = "Suburb", y = "Predicted Price")

# Predict future prices
future_data <- merged_data %>%
  select(Suburb_name, Projected_population_growth, Projected_median_income, 
         Projected_unemployment_rate, Projected_priority_growth_area) %>%
  rename(Historical_population_growth = Projected_population_growth,
         Historical_median_income = Projected_median_income,
         Historical_unemployment_rate = Projected_unemployment_rate,
         Historical_priority_growth_area = Projected_priority_growth_area)

# Predicted prices
d=future_prices <- predict(model, newdata = future_data)
d
# Calculate percentage increase
merged_data <- cbind(merged_data, Predicted_Price = future_prices)
merged_data <- merged_data %>%
  mutate(Percentage_Increase = (Predicted_Price - Median_price_2023) / Median_price_2023 * 100)

# Rank suburbs by percentage increase
top_suburbs <- merged_data %>%
  select(Suburb_name, Percentage_Increase) %>%
  arrange(desc(Percentage_Increase)) %>%
  head(10)

# Display top 10 suburbs by percentage increase
print(top_suburbs)

# Visualize top 10 suburbs by percentage increase
ggplot(top_suburbs, aes(x = reorder(Suburb_name, Percentage_Increase), y = Percentage_Increase)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 Suburbs by Percentage Increase in Predicted Price",
       x = "Suburb", y = "Percentage Increase")


