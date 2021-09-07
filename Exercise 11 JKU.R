# 1
# A continuous random variable has the density:
# f(x) = (1 - (x/10))*(1/5) when 0 ≤ x ≤ 10
# f(x) = 0 else
# a) Calculate P(3≤X≤9), P(X≥8) and P(|X-4| ≤ 3)
# b) Calculate the expected value and the variance of this random variable

# a)
# P(3≤X≤9) => Is the integral between 3 and 9 of f(x)

f <- function(x)((1 - (x/10))*(1/5))
prob <- integrate(f, lower = 3, upper = 9)
print(prob$value)

# P(X≥8) => Is the integral between 8 and 10 of f(x)

prob <- integrate(f, lower = 8, upper = 10)
print(prob$value)

# P(|X-4| ≤ 3) => if (1≤x≤7) => |X - 4| ≤ 3

prob <- integrate(f, lower = 1, upper = 7)
print(prob$value)

# b)
# µ = E(x) ∫x * f(x) between -∞ and ∞

f <- function(x)(x * ((1 - (x/10))*(1/5)))
E <- integrate(f, lower = 0, upper = 10)
print(E$value)

# Var(X) = E(x**2) - (E(X))**2

f2 <- function(x)((x**2) * ((1 - (x/10))*(1/5)))
E <- integrate(f, lower = 0, upper = 10)
E2 <- integrate(f2, lower = 0, upper = 10)

var <- E2$value - (E$value ** 2)
print(var)


# 2
# The magnesium content (mg/l) of bottled mineral water is normally distributed. Based
# on a sample of 6 bottles, the following values for the magnesium sontent (in mg/l) were gained:
# 23.2 23.2 24.1 25.1 23.3 22.3
# a) Calculate a 95% confident interval for the true mean magnesium content if the true variance
# is known and is ∂**2 = 0.900
# b) Calculate a 95% confidence interval for the true mean magnesium content, if the true variance
# is not known and has to be estimated on the given sample data.

# a)

X <- c(23.2, 23.2, 24.1, 25.1, 23.3, 22.3)
µ <- mean(X)
truevar <- 0.9
stddev <- sqrt(truevar)
n <- 6

percentage <- 0.95
upperbound <- 1 - ((1 -percentage)/2)
lowerbound <- (1 - percentage)/2

z <- qnorm(upperbound)
d <- (z * stddev) / sqrt(n)

uplimit <- µ + d 
downlimit <- µ - d
print(downlimit)
print(uplimit)

# b) 

var <- var(X)
stddev <- sqrt(var)

# alternative

v <- (sum(((X[1:n]) - µ)**2)) * (1 / (n-1))
stddev <- sqrt(v)

percentage <- 0.95
upperbound <- 1 - ((1 -percentage)/2)
lowerbound <- (1 - percentage)/2

t <- qt(upperbound, df=(n - 1))
d <- (t * stddev) / sqrt(n)

uplimit <- µ + d 
downlimit <- µ - d
print(downlimit)
print(uplimit)


# 3
# Someone owns a sawmill. He gets 1800 logs delivered. He wants to estimate the average
# circunference of the logs, with a reliability of 1-a = 95%
# The circunference of the logs is normally distributed. He now draws a random sample out
# of the 1800 delivered logs and measures the circunference x of each log in millimeters:
# 1110, 1150, 1200, 1430, 1100, 950, 1700, 1470
# a) Calculate the confidence interval based on the given sample data
# b) The sawmill owner now wants to determine a confidence interval for the average circumference
#   of the logs, which is 130mm wide and still has the same reliability of 95%.
#   Calculate the approximately needed sample size for this confidence interval.

# a)

X <- c(1110, 1150, 1200, 1430, 1100, 950, 1700, 1470)
µ <- mean(X)
n <- 8
variance <- var(X)
stddev <- sqrt(variance)

percentage <- 0.95
alpha <- 1 - percentage

t <- qt(1 - (alpha/2), df=(n - 1))
d <- (t * stddev) / sqrt(n)

uplimit <- µ + d 
downlimit <- µ - d
print(downlimit)
print(uplimit)

# b)

u <- qnorm(percentage)
L <- 130
nx <- ((2*u)*(stddev/L))**2
nx <- ceiling(nx)
print(nx)


# 4
# A bakery states that the biscuits it produces have an average weight of 50 grams. A statistics
# student who is suspicious buys n = 10 biscuits and weighs all biscuits. He receives the following
# biscuit weighs in grams:
# 46, 46, 49, 46, 48, 51, 49, 51, 53, 49
# a) Calculate an unbiased and consistent estimator for the true mean based on the given sample data.
# b) Calculate an unbiased and consistent estimator for the true variance based on the given sample data.
# c) Calculate a 90% confidence interval for the true mean weight of the biscuits based on the given sample data.

X <- c(46, 46, 49, 46, 51, 49, 51, 53, 49)
µ <- 50
n <- 10

# a)

meanest <- mean(X)
print(meanest)

# b)
varianceest <- var(X)
print(varianceest)

# c)

µ <- meanest
stddev <- sqrt(varianceest)

percentage <- 0.90
alpha <- 1 - percentage

t <- qt(1 - (alpha/2), df=(n - 1))
d <- (t * stddev) / sqrt(n)

uplimit <- µ + d 
downlimit <- µ - d
print(downlimit)
print(uplimit)

