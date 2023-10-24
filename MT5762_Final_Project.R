## Import CSV file data
setwd("/Users/lily/Desktop/英国留学材料/数据科学入门/project")
data <- read.csv("sales_data.csv")

## Part 2 a ****************************************************
## Calculate the proportion of sales revenue below 200
num_days_below_200 <- sum(data$icecream_sales < 200)
total_days <- nrow(data)
proportion_below_200 <- num_days_below_200 / total_days
## Calculate confidence intervals
se <- sqrt(proportion_below_200 * (1 - proportion_below_200) / total_days)
margin_error <- qnorm(0.975) * se
lower_bound <- proportion_below_200 - margin_error
upper_bound <- proportion_below_200 + margin_error

cat("Expected proportion of days with fewer than 200 ice cream sales:", proportion_below_200, "\n")
cat("95% Confidence Interval:", lower_bound, "-", upper_bound, "\n")


## Part 2 b ****************************************************
## Add a new column 'total'_ Sales'
data$total_sales <- data$icecream_sales + data$hotdrink_sales
## Calculate the proportion of days when total sales are less than 200
num_days_below_200 <- sum(data$total_sales < 200)
total_days <- nrow(data)
proportion_below_200 <- num_days_below_200 / total_days
## Calculate confidence intervals
se <- sqrt(proportion_below_200 * (1 - proportion_below_200) / total_days)
margin_error <- qnorm(0.975) * se
lower_bound <- proportion_below_200 - margin_error
upper_bound <- proportion_below_200 + margin_error

cat("Expected proportion of days with total sales under 200:", proportion_below_200, "\n")
cat("95% Confidence Interval:", lower_bound, "-", upper_bound, "\n")



## Part 2 c ****************************************************
## Locate the data for January and calculate the purchase quantity of ice cream and hot drinks
data_jan <- subset(data, month_name == "Jan")
num_icecream_jan <- sum(data_jan$icecream_sales)
num_hotdrink_jan <- sum(data_jan$hotdrink_sales)

## Calculate the probability ratio and confidence interval of purchasing ice cream instead of hot drinks in January
odds_ratio_jan <- num_icecream_jan / num_hotdrink_jan
se_jan <- sqrt(1 / num_icecream_jan + 1 / num_hotdrink_jan)
lower_bound_jan <- exp(log(odds_ratio_jan) - 1.96 * se_jan)
upper_bound_jan <- exp(log(odds_ratio_jan) + 1.96 * se_jan)

## Locate the data for August and calculate the purchase quantity of ice cream and hot drinks
data_aug <- subset(data, month_name == "Aug")
num_icecream_aug <- sum(data_aug$icecream_sales)
num_hotdrink_aug <- sum(data_aug$hotdrink_sales)

## Calculate the probability ratio and confidence interval of purchasing ice cream instead of hot drinks in August
odds_ratio_aug <- num_icecream_aug / num_hotdrink_aug
se_aug <- sqrt(1 / num_icecream_aug + 1 / num_hotdrink_aug)
lower_bound_aug <- exp(log(odds_ratio_aug) - 1.96 * se_aug)
upper_bound_aug <- exp(log(odds_ratio_aug) + 1.96 * se_aug)

cat("Odds ratio for purchase being ice cream rather than hot drink in Jan:", odds_ratio_jan, "\n")
cat("95% Confidence Interval for January:", lower_bound_jan, "-", upper_bound_jan, "\n")

cat("Odds ratio for purchase being ice cream rather than hot drink in Aug:", odds_ratio_aug, "\n")
cat("95% Confidence Interval for August:", lower_bound_aug, "-", upper_bound_aug, "\n")

## Part 2 d ****************************************************
## Result：there is a significant difference in odds ratios between January and August.



## Part 3 a ****************************************************
## Split data into weekdays and weekends
weekdays_data <- subset(data, weekend == 0)$total_sales
weekend_data <- subset(data, weekend == 1)$total_sales

## Conduct hypothesis testing
result <- t.test(weekdays_data, weekend_data)

print(result)
## box-plot
library(ggplot2)

sales_data <- data.frame(
  Sales = c(weekdays_data, weekend_data),
  Day_Type = c(rep("Weekdays", length(weekdays_data)), rep("Weekend", length(weekend_data)))
)
## Draw a box diagram
ggplot(sales_data, aes(x = Day_Type, y = Sales, fill = Day_Type)) +
  geom_boxplot() +
  labs(title = "Difference between the expected number of sales on weekdays and weekends",
       x = "Day Type", y = "Sales") +
  theme_minimal()


## Part 3 b ****************************************************
## install.packages("pwr")
library(pwr)
## Calculate the effect quantity
mean_difference <- 600.0588 - 324.4038
standard_deviation <- sqrt(var(weekend_data) / length(weekend_data) + var(weekdays_data) / length(weekdays_data))
effect_size <- mean_difference / standard_deviation
effect_size
## Calculate power
power <- pwr.t.test(d = effect_size, n = length(weekdays_data) + length(weekend_data), sig.level = 0.05, type = "two.sample", alternative = "two.sided")$power
power



## Part 3 c ****************************************************
## Define known parameters
alpha <- 0.05  
power <- 0.9  
n <- length(weekdays_data)  

## Calculate the required amount of effect using the pwr. t. test function
effect_size_needed <- pwr.t.test(n = n, sig.level = alpha, power = power, type = "two.sample")$d
effect_size_needed


## Part 3 d ****************************************************
## Define known parameters
alpha <- 0.05  
power <- 0.9  
d <- 0.6417859  

## Calculate the required sample size using the pwr. t. test function
sample_size_needed <- pwr.t.test(d = d, sig.level = alpha, power = power, type = "two.sample")$n
sample_size_needed



## Part 4 ****************************************************
## Establishing a Multiple Linear Regression Model
model <- lm(icecream_sales ~ temperature + humidity + windspeed + weekend + bank_holiday + school_holidays, data = data)

## Estimated parameters of the model
summary(model)

## a.
new_data_a <- data.frame(temperature = 18, humidity = 0.06, windspeed = 10, weekend = 0, bank_holiday = 0, school_holidays = 0)
predict_a <- predict(model, newdata = new_data_a, interval = "confidence")

## b.
new_data_b <- data.frame(temperature = 28, humidity = 0.35, windspeed = 5, weekend = 1, bank_holiday = 0, school_holidays = 1)
predict_b <- predict(model, newdata = new_data_b, interval = "confidence")

## c.
new_data_c <- data.frame(temperature = 12, humidity = 0.9, windspeed = 35, weekend = 0, bank_holiday = 0, school_holidays = 0)
predict_c <- predict(model, newdata = new_data_c, interval = "confidence")

## d.
new_data_d <- data.frame(temperature = -2, humidity = 0.75, windspeed = 15, weekend = 1, bank_holiday = 0, school_holidays = 0)
predict_d <- predict(model, newdata = new_data_d, interval = "confidence")

## Output prediction results and confidence intervals
print(predict_a)
print(predict_b)
print(predict_c)
print(predict_d)


## Draw residual plot
plot(model, which = 1)  

## Draw QQ diagram
qqnorm(residuals(model))  
qqline(residuals(model))  

## Observing the significance level of each independent variable
summary(model)









