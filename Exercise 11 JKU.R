# 1
# A continuous random variable has the density:
# f(x) = (1 - (x/10))*(1/5) when 0 ≤ x ≤ 10
# f(x) = 0 else
# a) Calculate P(3≤X≤9), P(X≥8) and P(|X-4| ≤ 3)
# b) Calculate the expected value and the variance of this random variable

cat("Excercise 1")

# a)
# P(3≤X≤9) => Is the integral between 3 and 9 of f(x)

f <- function(x)((1 - (x/10))*(1/5))
prob <- integrate(f, lower = 3, upper = 9)
cat("a)")
cat("P(3≤X≤9): ", round(prob$value * 100, 2), "%")

# P(X≥8) => Is the integral between 8 and 10 of f(x)

prob <- integrate(f, lower = 8, upper = 10)
cat("P(X≥8): ", round(prob$value * 100, 2), "%")

# P(|X-4| ≤ 3) => if (1≤x≤7) => |X - 4| ≤ 3

prob <- integrate(f, lower = 1, upper = 7)
cat("P(|X-4| ≤ 3): ", round(prob$value * 100, 2), "%")
cat("\n")


# b)
# µ = E(x) ∫x * f(x) between -∞ and ∞

cat("b)")

f <- function(x)(x * ((1 - (x/10))*(1/5)))
E <- integrate(f, lower = 0, upper = 10)
cat("E(x): ", E$value)

# Var(X) = E(x**2) - (E(X))**2

f2 <- function(x)((x**2) * ((1 - (x/10))*(1/5)))
E <- integrate(f, lower = 0, upper = 10)
E2 <- integrate(f2, lower = 0, upper = 10)

var <- E2$value - (E$value ** 2)
cat("var(x): ", var)


# 2
# The magnesium content (mg/l) of bottled mineral water is normally distributed. Based
# on a sample of 6 bottles, the following values for the magnesium sontent (in mg/l) were gained:
# 23.2 23.2 24.1 25.1 23.3 22.3
# a) Calculate a 95% confident interval for the true mean magnesium content if the true variance
# is known and is ∂**2 = 0.900
# b) Calculate a 95% confidence interval for the true mean magnesium content, if the true variance
# is not known and has to be estimated on the given sample data.

cat("Excercise 2")

# a)

cat("a)")

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
cat("P(95%): (",downlimit, ",", uplimit, ")")

# b) 

cat("b)")

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
cat("P(95%): (",downlimit, ",", uplimit, ")")


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

cat("Excercise 3")

# a)

cat("a)")

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
cat("P(95%): (",downlimit, ",", uplimit, ")")

# b)

cat("b)")

u <- qnorm(1 - (alpha/2))
L <- 130
nx <- ((2*u)*(stddev/L))**2
nx <- ceiling(nx)
cat("Sample size:", nx)


# 4
# A bakery states that the biscuits it produces have an average weight of 50 grams. A statistics
# student who is suspicious buys n = 10 biscuits and weighs all biscuits. He receives the following
# biscuit weighs in grams:
# 46, 46, 49, 46, 48, 51, 49, 51, 53, 49
# a) Calculate an unbiased and consistent estimator for the true mean based on the given sample data.
# b) Calculate an unbiased and consistent estimator for the true variance based on the given sample data.
# c) Calculate a 90% confidence interval for the true mean weight of the biscuits based on the given sample data.

cat("Excercise 4")

X <- c(46, 46, 49, 46, 48, 51, 49, 51, 53, 49)
µ <- 50
n <- 10

# a)
cat("a)")

meanest <- mean(X)
cat("Estimated mean:", meanest)

# b)
cat("b)")

varianceest <- var(X)
cat("Estimated variance:", varianceest)

# Alternative
v <- (sum(((X[1:n]) - meanest)**2)) * (1 / (n-1))
cat("Alternative method:")
cat("Estimated variance:", v)

# c)
cat("c)")

µ <- meanest
stddev <- sqrt(varianceest)

percentage <- 0.90
alpha <- 1 - percentage

t <- qt(1 - (alpha/2), df=(n - 1))
d <- (t * stddev) / sqrt(n)

uplimit <- µ + d 
downlimit <- µ - d
cat("P(", percentage*100, "%): (",downlimit, ",", uplimit, ")")


# 5
# A study about the birth weight of premature infants is being conducted in a clinic.
# The birth weight of an infant in the 28th week of pregnancy is assumed to be a normally
# distributed random variable with a mean of 950g and a standard deviation of 50g.
# a) What is the probability that an infant born in the 28th week of pregnancy will weigh
#   between 980g and 1040g?
# b) Calculate the 25% quantile of the random variable birth weight.

cat("Excercise 5")

µ <- 950
stddev <- 50

# a)
# P(980 ≤ x ≤ 1040) = P(x ≤ 1040) - P(x ≤ 980)

cat("a)")

x1 <- 1040
x2 <- 980

prob1 <- pnorm(x1, µ, stddev)
prob2 <- pnorm(x2, µ, stddev)
prob <- prob1 - prob2

cat("P(", x2, "≤ x ≤", x1, "):", round(prob * 100, 2), "%")


# b)

cat("b)")

percentage <- 0.25
x <- qnorm(percentage, µ, stddev)
cat(percentage * 100, "% quantile:", round(x, 2))


# 6
# A continuous random variable X has the density f(x)=2*(x-2) for 2 ≤ x ≤ 3 (0 else)
# a) Calculate the expected value and the variance of this random variable
# b) Above which value are 60% of the distribution mass of this random variable

cat("Excercise 6")

# a)

cat("a)")

f <- function(x)(x*(2*(x-2)))
ev <- integrate(f, lower = 2, upper = 3)
cat("E(x): ", ev$value)

f2 <- function(x)((x**2)*(2*(x-2)))
E2 <- integrate(f2, lower = 2, upper = 3)
var <- E2$value - (ev$value ** 2)
cat("var(x): ", var)

# b)

# ∫2*(x-2) between x and 3 = 0.6
# (x ** 2) - 4 * x between x and 3 = 0.6
# -3 - x ** 2 + 4 * x - 0.6 = 0
#  - x ** 2 + 4 * x - 3.6 = 0

cat("b)")

calcquad <- function(a, b, c){
  x <- c()
  x <- c(x, ((-b + sqrt((b**2) - (4 * a * c))) / 2 * a))
  x <- c(x, ((-b - sqrt((b**2) - (4 * a * c))) / 2 * a))
  return (x)
}

solution <- calcquad(-1, 4, - 3.6)

if (solution[1] <= 3 & solution[1] >= 2){
  cat("P(x ≤", solution[1], ") = 60%")
}
if (solution[2] <= 3 & solution[2] >= 2){
  cat("P(x ≤", solution[2], ") = 60%")
}


# 7
# On behalf of the owner of a winery, the true average bottle quantity of wine, which is
# bottled in 750ml wine bottles, should be estimated based on a 99% confidence interval.
# The filling quantity X is regarded as normally distributed. Ten bottles are randomly
# selected, and the filling quantity of these bottles is checked.
# Quantity of wine (ml) of the sample:
# 760 756 748 745 745 755 748 760 755 770
# a) Calculate the confidence interval based on the given sample data
# b) Which confidence level has to be chosen, so that with a sample of 40 bottles a confidence
#   interval with a length of at most 1 millimeter is achieved? An approximate solution is sufficient

cat("Excercise 7")

X <- c(760, 756, 748, 745, 745, 755, 748, 760, 755,770)
µ <- 750
variance <- var(X)
stddev <- sqrt(variance)
n <- length(X)

# a)

cat("a)")

percentage <- 0.99
alpha <- 1 - percentage

t <- qt(1 - (alpha/2), df=(n - 1))
d <- (t * stddev) / sqrt(n)

uplimit <- µ + d 
downlimit <- µ - d

cat("P(", percentage*100, "%): (",downlimit, ",", uplimit, ")")


# b)

cat("b)")

# 8
# We know that the distribution of the weight of a certain bat species follows a normal
# distribution with mean µ = 120grams and a variance of 400grams**2
# Calculate the limits of an interval, which covers the central 90% of the distribution.

cat("Excercise 8")

µ <-  120
variance <- 400
stddev <-  sqrt(variance)
percentage <- 0.9
n <- 6

upperbound <- 1 - ((1 - percentage)/2)
lowerbound <- (1 - percentage)/2

z <- qnorm(upperbound)
d <- z * stddev

uplimit <- µ + d 
downlimit <- µ - d

cat("P(", percentage*100, "%): (",downlimit, ",", uplimit, ")")



# 9
# Someone has estimated the mean and the variance of the weight of another bat species based
# on a sample of 10 bats. The sample mean was 120 grams and the sample variance was 400 grams^2.
# Calculate a 90% confidence interval for the true unknown mean.

cat("Excercise 9")

µest <-120
varianceest <- 400
stddev <- sqrt(varianceest)
n <- 10

percentage <- 0.9
alpha <- 1 - percentage

t <- qt(1 - (alpha/2), df=(n - 1))
d <- (t * stddev) / sqrt(n)

uplimit <- µest + d 
downlimit <- µest - d

cat("P(", percentage*100, "%): (",downlimit, ",", uplimit, ")")


# 10
# 41 people were asked to measure their pulse rates after completing a 5km run. The mean was
# 105 beats and the standard deviation was 8 beats.
# a) Construct a 95% confidence interval for the true mean based on this sample
# b) Now 600 people were asked, the mean and variance of this sample are remainig the same.
#   Construct a 95% confidence interval for the true mean based on this sample.
# c) Assume nor only 6 people were asked, the mean and variance of this sample are still remaining
#   the same. Construct a 95% confidence interval for the true mean based on this sample.

cat("Excercise 10")

µ <- 105
stddev <- 8

# a)

cat("a)")

percentage <- 0.95
n <- 41
alpha <- 1 - percentage

t <- qt(1 - (alpha/2), df=(n - 1))
d <- (t * stddev) / sqrt(n)

uplimit <- µ + d 
downlimit <- µ - d

cat("P(", percentage*100, "%): (",downlimit, ",", uplimit, ")")

# b)

cat("b)")

n <- 600

t <- qt(1 - (alpha/2), df=(n - 1))
d <- (t * stddev) / sqrt(n)

uplimit <- µ + d 
downlimit <- µ - d

cat("P(", percentage*100, "%): (",downlimit, ",", uplimit, ")")

# c)

cat("c)")

n <- 6

t <- qt(1 - (alpha/2), df=(n - 1))
d <- (t * stddev) / sqrt(n)

uplimit <- µ + d 
downlimit <- µ - d

cat("P(", percentage*100, "%): (",downlimit, ",", uplimit, ")")

# 11
# A new tax law is expected to benefit "middle income" families, those with incomes between
# 20,000$ and 30,000$. If family income follows a normal distribution with µ = 25,000 and 
# stddev = 10,000$. What percentage of the population will benefit from this law?

cat("Excercise 11")

µ <-  25000
stddev <- 10000

x1 <- 20000
x2 <- 30000

prob1 <- pnorm(x1, µ, stddev)
prob2 <- pnorm(x2, µ, stddev)
prob <- prob2 - prob1

cat("P(", x1, "≤ x ≤", x2, "):", round(prob * 100, 2), "%")


# 12
# A political candidate finds that in a random sample of 300 constituents, 34% support her party.
# Calculate the 95% confidence interval for the support she in fact has.

cat("Excercise 12")

n <- 300