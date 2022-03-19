# ISLR 3.7 Applied Problem 8 (page 123)
Auto = read.csv("/home/dtariq1995/Desktop/MSCS/Spring 2022/CS5565 ISL/CS5565-Lab1/Data/Auto.csv", header = TRUE, na.strings = "?")
Auto = na.omit(Auto)

# A: Using lm() to perform simple linear regression with mpg as response and hp as predictor. Print results using summary(). Comment on output.
auto.lin.fit = lm(mpg ~ horsepower, data = Auto)
summary(auto.lin.fit)
# A123: The p-value of -0.157845 means that each increase in 1 hp results in a 0.157845 decrease in mpg so there's a negative relationship between horsepower and mpg
predict(auto.lin.fit, data.frame(horsepower = 98), interval = "confidence")
predict(auto.lin.fit, data.frame(horsepower = 98), interval = "prediction")
# A4: The predicted mpg with hp of 98 is 24.46708. The 95% confidence interval is (23.97308, 24.96108) and the 95% prediction interval is (14.8094, 34.12467)

# B: Plot response and predictor and use abline() to display least squares regression line
plot(Auto$horsepower, Auto$mpg, xlab = "Horsepower", ylab = "Miles per gallon")
abline(auto.lin.fit, lwd = 3, col = "red")

# C: Use plot() to produce diagnostic plots of least squares regression fit. Comment on any issues with fit. 
par(mfrow = c(2, 2))
plot(auto.lin.fit)
# Comments: Looking at these plots it's pretty obvious that linear regression model isn't a good fit for all plots except for the Normal Q-Q plot


#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# ISLR 3.7 Applied Problem 9 (page 123)

Auto$origin[Auto$origin == 1] = "American"
Auto$origin[Auto$origin == 2] = "European"
Auto$origin[Auto$origin == 3] = "Japanese"
Auto$origin = as.factor(Auto$origin)
head(Auto)

# A: Produce a scatterplot matrix which includes all variables in the dataset (categorical columns are excluded)
pairs(~mpg + cylinders + displacement + horsepower + weight + acceleration + year, Auto)

# B: Compute matrix of correlations between variables using the function cor(). Exclude name variable.
cor(Auto[,-c(8, 9)])

# C: Use lm() and perform multiple linear regression with mpg as response and other variables (excluding name) as predictors. Use summary() to print results.
mpg.fit = lm(mpg ~ . - name, data = Auto)
summary(mpg.fit)
contrasts(Auto$origin)
# C12: There's a significant relation to the response mpg from the predictors displacement, weight, year, originEuropean, and oringJapanese. The p value isn't small enough for cylinders, hp, and acceleration to show a relationship. 
# C3: The 0.777 coefficient for Year tells us that if we have a fixed number for cylinders, displacement, hp, weight, acceleration, and origin, the fuel efficiency increases by 0.777 mpg per year. 

# D: Use plot() to make diagnostic plots of the linear regression fit. Comment on any issues with fit. Do residual plots suggest unusually large outliers? Do leverage plots identify observations with unusually high leverage?
par(mfrow = c(2, 2))
plot(mpg.fit)
# Comment: U-shape in R vs F graph shows that there might be non-linearity. There's also unusually high outliers. 

# E: Use * and : symbols to fit linear regression models with interaction effects. Are any interactions statistically significant?
mpg.fit.all.interactions = lm(mpg ~ (. - name)^2, data = Auto)
summary(mpg.fit.all.interactions)

# 6: Try a few different transformations of the variables
par(mfrow = c(2, 2))
plot(Auto$displacement, Auto$mpg)
plot(Auto$horsepower, Auto$mpg)
plot(Auto$weight, Auto$mpg)
plot(Auto$acceleration, Auto$mpg)
displacement.quintic = lm(mpg ~ poly(displacement, 5), data = Auto)
summary(displacement.quintic)

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# ISLR 3.7 Applied Problem 10 (page 123)

library(ISLR)
head(Carseats)

# A: Fit a multiple regression model to predict Sales using Price, Urban, and US.
carseats.fit.1 = lm(Sales ~ Price + Urban + US, data = Carseats)
summary(carseats.fit.1)

contrasts(Carseats$Urban)

contrasts(Carseats$US)

# B: Provide an interpretation of each coeficient in the model. 
# The coefficient of -0.054459 for Price means that increasing price of car seat by $1 results in a decrease of sales by 54.46 units. 
# The coefficient of -0.021916 for UrbanYes means urban areas are predicted to have 22% fewer carseat sales compared to non-urban areas. 
# The coefficient of 1.200573 for USYes means that stores in US on average have 1201 more carseat sales than stores outside of the United States.

# C: Write out the model in equation form. --> Y^=13.043−0.054X1−0.022X2+1.200X3

# D: We can reject the null hypothesis for Price and USYes

# E: Fit a smaller model that excludes Price and USYes
carseats.fit.2 = lm(Sales ~ Price + US, data = Carseats)
summary(carseats.fit.2)

# F: How well do the models in A and E fit the data? They fit equally well, with identical R2 values
carseats.fit.2 = lm(Sales ~ Price + US, data = Carseats)
summary(carseats.fit.2)

par(mfrow = c(2, 2))
plot(carseats.fit.2)

# G: Get 95% confidence intervals for coefficients
confint(carseats.fit.2)

# H: Is there evidence of outliers in model from part 5? Yes, we can see clearly that there are outliers and high leverage points