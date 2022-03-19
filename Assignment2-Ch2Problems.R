# ISLR 2.4 Applied Problem 8

# read in data from college.csv file
college = read.csv("/home/dtariq1995/Desktop/MSCS/Spring 2022/CS5565 ISL/CS5565-Lab1/Data/College.csv", header = TRUE)
head(college)
rownames(college) = college[, 1]
college = college[, -1]
head(college)

# Using summary function to produce summary of variables in dataset
summary(college)
college$Private <- as.factor(college$Private)
# Using pairs function to produce scatterplot matrix of first 10 columns or variables
pairs(college[, 1:10])

plot(college$Private, college$Outstate, xlab = "Private", ylab = "Out-of-state tuition (dollars)")

# Sort universities based on how many of uni students are made up of top 10% high school performers
Elite = rep("No", nrow(college))
Elite[college$Top10per > 50] = "Yes"
Elite = as.factor(Elite)
college = data.frame(college, Elite)

# Use summary to see # of elite universities and then use plot() to make boxplots showing outstate versus elite
summary(college$Elite)
plot(college$Elite, college$Outstate, xlab = "Elite", ylab = "Out-of-state tuition (dollars)")

# Use hist() to make histograms with differing number of bins for few of the quantitative variables
par(mfrow = c(2, 2))
hist(college$Apps, xlab = "Number of applicants", main = "Histogram for all colleges")
hist(college$Apps[college$Private == "Yes"], xlab = "Number of applicants", main = "Histogram for private schools")
hist(college$Apps[college$Private == "No"], xlab = "Number of applicants", main = "Histogram for public schools")
hist(college$Apps[college$Elite == "Yes"], xlab = "Number of applicants", main = "Histogram for elite schools")

par(mfrow = c(2, 2))
hist(college$Expend, xlab = "Instructional expenditure per student (dollars)", main = "Histogram for all colleges")
hist(college$Expend[college$Private == "Yes"], xlab = "Instructional expenditure per student (dollars)", main = "Histogram for private schools")
hist(college$Expend[college$Private == "No"], xlab = "Instructional expenditure per student (dollars)", main = "Histogram for public schools")
hist(college$Expend[college$Elite == "Yes"], xlab = "Instructional expenditure per student (dollars)", main = "Histogram for elite schools")

# Continue analysis of data and provide brief summary of discoveries
NonTuitionCosts = college$Room.Board + college$Books + college$Personal
college = data.frame(college, NonTuitionCosts)
par(mfrow = c(1, 2))
plot(college$Private, college$NonTuitionCosts, xlab = "Private", ylab = "Total non-tuition costs per year (dollars)")
plot(college$Elite, college$NonTuitionCosts, xlab = "Elite", ylab = "Total non-tuition costs per year (dollars)")

AcceptPerc = college$Accept / college$Apps * 100
college = data.frame(college, AcceptPerc)
par(mfrow = c(1, 2))
plot(college$Private, college$AcceptPerc, xlab = "Private", ylab = "Acceptance Rate")
plot(college$Elite, college$AcceptPerc, xlab = "Elite", ylab = "Acceptance Rate")

summary(college$AcceptPerc[college$Private == "Yes"])
summary(college$AcceptPerc[college$Private == "No"])
summary(college$AcceptPerc[college$Elite == "Yes"])
summary(college$AcceptPerc[college$Elite == "No"])

par(mfrow = c(2, 2))
hist(college$perc.alumni, xlab = "Percent of alumni who donate", main = "Histogram for all colleges")
hist(college$perc.alumni[college$Private == "Yes"], xlab = "Percent of alumni who donate", main = "Histogram for private schools")
hist(college$perc.alumni[college$Private == "No"], xlab = "Percent of alumni who donate", main = "Histogram for public schools")
hist(college$perc.alumni[college$Elite == "Yes"], xlab = "Percent of alumni who donate", main = "Histogram for elite schools")

par(mfrow = c(2, 2))
plot(college$PhD, college$Grad.Rate, xlab = "Number of faculty with PhDs", ylab = "Graduation Rate")
plot(college$Terminal, college$Grad.Rate, xlab = "Number of faculty with terminal degrees", ylab = "Graduation Rate")
plot(college$S.F.Ratio, college$Grad.Rate, xlab = "Student-faculty ratio", ylab = "Graduation Rate")
plot(college$Expend, college$Grad.Rate, xlab = "Instructional expenditure per student (dollars)", ylab = "Graduation Rate")

#----------------------------------------------------------------------------------------------------------------------------------------------------

# ISLR 2.4 Applied Problem 9 (page 56)


# Read in Auto.csv and remove missing values from data
Auto = read.csv("/home/dtariq1995/Desktop/MSCS/Spring 2022/CS5565 ISL/CS5565-Lab1/Data/Auto.csv", header = TRUE, na.strings = "?")
Auto = na.omit(Auto)
dim(Auto)

# Part1 - which predictors are quantitative, which are qualitative? 
# quantitative - mpg, displacement, horsepower, weight, acceleration
# qualitative - origin, name
head(Auto)

# Part2: Finding range for each quantitative predictor
?range
range(Auto$mpg)
range(Auto$cylinders)
range(Auto$horsepower)
range(Auto$weight)
range(Auto$acceleration)
range(Auto$year)

# Part 3: Find mean and standard deviation of each quantitative predictor
colMeans(Auto[, 1:7])
apply(Auto[,1:7], MARGIN = 2, FUN = "sd")

 # Part 4: Remove 10th thru 85th observations and return range, mean, and std dev for remaining data
apply(Auto[-(10:85), 1:7], MARGIN = 2, FUN = "range")
apply(Auto[-(10:85), 1:7], MARGIN = 2, FUN = "mean")
apply(Auto[-(10:85), 1:7], MARGIN = 2, FUN = "sd")

# Part 5: Using full data set, use plots of your choice to look at predictors graphically
par(mfrow = c(2, 2))
plot(Auto$displacement, Auto$mpg, xlab = "Engine displacement (cubic inches)", ylab = "Miles per gallon")
plot(Auto$horsepower, Auto$mpg, xlab = "Horsepower", ylab = "Miles per gallon")
plot(Auto$weight, Auto$mpg, xlab = "Car weight (pounds)", ylab = "Miles per gallon")
plot(Auto$year, Auto$mpg, xlab = "Model Year", ylab = "Miles per gallon")
# these plots show that cars that weigh less, are newer, a weaker engine, and less hp are more fuel efficient
par(mfrow = c(2, 2))
plot(Auto$year, Auto$acceleration, xlab = "Model Year", ylab = "0 to 60mph time (seconds)")
plot(Auto$year, Auto$displacement, xlab = "Model Year", ylab = "Engine displacement (cubic inches)")
plot(Auto$year, Auto$weight, xlab = "Model Year", ylab = "Car weight (pounds)")
plot(Auto$year, Auto$horsepower, xlab = "Model Year", ylab = "Horsepower")
# acceleration was quicker, engine displacement lower, weight was lower, and hp was lower for newer cars
par(mfrow = c(2, 2))
plot(Auto$weight, Auto$acceleration, xlab = "Car weight (pounds)", ylab = "0 to 60mph time (seconds)")
plot(Auto$cylinders, Auto$acceleration, xlab = "Number of engine cylinders", ylab = "0 to 60mph time (seconds)")
plot(Auto$displacement, Auto$acceleration, xlab = "Engine displacement (cubic inches)", ylab = "0 to 60mph time (seconds)")
plot(Auto$horsepower, Auto$acceleration, xlab = "Horsepower", ylab = "0 to 60mph time (seconds)")
# 0-60 is was slower for heavier cars, most cars with more cylinders were quicker, so were cars with more hp and engine displacement
par(mfrow = c(2, 1))
plot(Auto$weight, Auto$horsepower, xlab = "Car weight (pounds)", ylab = "Horsepower")
plot(Auto$weight, Auto$displacement, xlab = "Car weight (pounds)", ylab = "Engine displacement (cubic inches)")
# Horsepower and engine displacement trend upwards as the the car weight goes up

# Part 6: Looking at other plots, are there any other variables that may be useful for predicting mpg? 
Auto$origin[Auto$origin == 1] = "American"
Auto$origin[Auto$origin == 2] = "European"
Auto$origin[Auto$origin == 3] = "Japanese"
Auto$origin = as.factor(Auto$origin)
plot(Auto$origin, Auto$mpg, xlab = "Country of origin", ylab = "Miles per gallon")
# Looking at this plot we can see that the car's country of origin seems to play a role on fuel efficiency as well

#----------------------------------------------------------------------------------------------------------------------------------------------------

# ISLR 2.4 Applied Problem 10 (page 57)

# Part 1: Loading Boston Dataset 
library(MASS)
head(Boston)
?Boston
# Switching over to the correct Boston data set
Boston_corrected = read.csv("/home/dtariq1995/Desktop/MSCS/Spring 2022/CS5565 ISL/CS5565-Lab1/Data/boston_corrected.csv", header = TRUE)
head(Boston_corrected)
dim(Boston_corrected)

# Part 2: Make some pairwise scatterplots of the predictors in the data set.
dim(Boston_corrected)
par(mfrow = c(2, 2))
plot(Boston_corrected$AGE, Boston_corrected$CMEDV, xlab = "Percent of units built prior to 1940", ylab = "Median home value in $1000s")
plot(Boston_corrected$LSTAT, Boston_corrected$CMEDV, xlab = "Percent of lower status residents", ylab = "Median home value in $1000s")
plot(Boston_corrected$CMEDV, Boston_corrected$PTRATIO, xlab = "Median home value in $1000s", ylab = "Pupil-teacher ratio")
plot(as.factor(Boston_corrected$CHAS), Boston_corrected$CMEDV, xlab = "Borders Charles River", ylab = "Median home value in $1000s")
# There's no obvious pattern that can be found in these graphs 
par(mfrow = c(2, 2))
plot(Boston_corrected$CMEDV, Boston_corrected$NOX, xlab = "Median home value in $1000s", ylab = "Nitric oxides concentration (parts per 10 million)")
plot(Boston_corrected$INDUS, Boston_corrected$NOX, xlab = "Percent of non-retail business acres", ylab = "Nitric oxides concentration (parts per 10 million)")
plot(Boston_corrected$CMEDV, Boston_corrected$B, xlab = "Median home value in $1000s", ylab = "1000(Proportion of black residents - 0.63)^2")
plot(Boston_corrected$DIS, Boston_corrected$CMEDV, xlab = "Weighted distance to Boston employment centers", ylab = "Median home value in $1000s")
# From these plots I can see that higher value homes tend to have lower nitric oxide concentration

# Part 3: Are any predictors associated with per capita crime rate? 
par(mfrow = c(2, 2))
plot(Boston_corrected$B, Boston_corrected$CRIM, xlab = "1000(Proportion of black residents - 0.63)^2", ylab = "Per capita crime rate")
plot(Boston_corrected$LSTAT, Boston_corrected$CRIM, xlab = "Percent of lower status residents", ylab = "Per capita crime rate")
plot(Boston_corrected$CMEDV, Boston_corrected$CRIM, xlab = "Median home value in $1000s", ylab = "Per capita crime rate")
plot(Boston_corrected$DIS, Boston_corrected$CRIM, xlab = "Weighted distance to Boston employment centers", ylab = "Per capita crime rate")
# From these scatterplots it looks like there's a correlation between crime rate and home value, distance to employment centers, and lower status residents

# Part 4: Do any of the suburbs have particularly high crime rates? Tax rates? Pupil-teacher ratios?
par(mfrow = c(2, 2))
hist(Boston_corrected$CRIM, xlab = "Per capita crime rate", main = "Histogram of Boston crime rates")
hist(Boston_corrected$TAX, xlab = "Tax rate per 10000 USD", main = "Histogram of Boston tax rates")
hist(Boston_corrected$PTRATIO, xlab = "Pupil-teacher ratio", main = "Histogram of Boston pupil-teacher ratios")
summary(Boston_corrected[, c(8, 17, 18)])
# Based on the histograms and summary there are tracts within boston which have higher ratios of these, but the average is still normal

# Part 5: How many of the suburbs bound the Charles river?
sum(Boston_corrected$CHAS)

# Part 6: What is the median pupil-teacher ratio among towns in this data set? 
summary(Boston_corrected$PTRATIO)

# Part 7: Which suburb of Boston has the lowest median value of owner-occupied homes? 
min(Boston_corrected$CMEDV)
Boston_corrected[Boston_corrected$CMEDV == 5, ]
summary(Boston_corrected[, c(8:10, 12:20)])

# Part 8: How many suburbs average more than seven rooms per dwelling?
sum(Boston_corrected$RM > 7)
sum(Boston_corrected$RM > 8)
Boston_corrected[Boston_corrected$RM > 8, ]
summary(Boston_corrected[Boston_corrected$RM > 8, c(7:10, 12:20)])