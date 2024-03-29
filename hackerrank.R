#HackerRank 10 days Statistic

# This is a function only to use in RStudio, when you resolve the exercise in 
# Hackerrank you dont need it.
# Function to read a string from the command line, split into numbers and 
# return a vector.

readcommand <- function(arr){
  arr <- readLines(stdin(), n=1)
  arr <- strsplit(arr, " ")
  arr <- as.numeric(unlist(arr))
}

# Day 1. 1
#Calculate Mean, median, mode

n <- readLines(stdin(), n=1)
n <- as.numeric(n)

a <- readcommand()

mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

mean(a)
median(a)
mode(a)

# Day 1. 2
# Calculate Weighted mean

X <- readcommand()

W <- readcommand()

c <- weighted.mean(X, W)

h <- round(c, 1)
cat(h)

# Day 2.1
# Calculate Quartiles

n <- readLines(stdin(), n=1)
n <- as.numeric(n)

a <- readcommand()

b <- quantile(a)

fq <- sprintf(b[2], fmt = '%#.1f')
sq <- sprintf(b[3], fmt = '%#.1f')
tq <- sprintf(b[4], fmt = '%#.1f')

cat(fq)
cat(sq)
cat(tq)

# Day 2.2
# Interquartile Range
#

X <- readcommand()

W <- readcommand()

S <- c()

for (i in 1:length(W)){
  for (j in 1:W[i]){
    S <- c(S, X[i])
  }
}

IQR (S)

# Day 2.3
# Standard Deviation
#

X <- readcommand()

error <- c()

for (i in 1:length(X)){
  error <- c(error, (mean(X) - X[i]) ** 2)
}

my_stddev <- round(sqrt(sum(error) / length(X)), 1)
cat(my_stddev)

# Day 3.1
# Basic Probability
# Probability of a single toss of two dices their sum is at most 9

probset <- c(1/36, 2/36, 3/36, 4/36, 5/36, 6/36, 5/36, 4/36)

totalprob <- sum(probset)

# The answer is 5/6

# Day 3.2
# In a single toss of  fair (evenly-weighted) six-sided dice, 
# find the probability that the values rolled by each die will 
# be different and the two dice have a sum of 6.

# P(A) A = Numbers are different
probdifferent <- 30/36
# P(B) B = Sum is six
prob6 <- 5/36
# P(A|B) A|B = Numbers are different if sum is 6
probdifferentif6 <- 4/5
# P(A and B) A and B = Numbers are different and sum is six
# P(A and B) = P(A|B) * P(B) = 4/5 * 5/6
probdiffand6 <- probdifferentif6 * prob6
# Answer is 4/36 = 1/9


# Day 3.3
# There are  urns labeled X, Y, and Z. 
# Urn X contains 4 red balls and 3 black balls.
# Urn Y contains 5 red balls and 4 black balls.
# Urn Z contains 4 red balls and 4 black balls. 
# One ball is drawn from each of the 3 urns. What is the probability that, 
# of the 3 balls drawn, 2 are red and 1 is black?

# First urn red = 4/7 black = 3/7
# Second urn red = 5/9 black = 4/9
# Third urn red = 1/2 black = 1/2
# Possiblities:
# rrb rbr brr
# rrb = 4/7 * 5/9 * 1/2 = 20/126
# rbr = 4/7 * 4/9 * 1/2 = 16/126
# brr = 3/7 * 5/9 * 1/2 = 15/126
# Total = rrb + rbr + brr = 51/126 = 17/42

# Day 4.1
# Conditional probability
# Suppose a family has  children, one of which is a boy. 
# What is the probability that both children are boys?
# Probability of 2 Boy if we know 1 is a Boy
# P (2B | 1B) = P (2B and 1B) / P(1B)
# P (2B and 1B) = 1/4
# P (1/B) = 3/4
# P (2B | 1B) = 1/4 / 3/4 = 1/3

# Day 4.2
# You draw 2 cards from a standard 52-card deck without replacing them. 
# What is the probability that both cards are of the same suit?
# First card could be any card. So, probability of 1
# Second card we have 12 possibilities out of 51.
# P = 1 * 12/51= 12/51

# Day 4.3
# A bag contains 3 red marbles and 4 blue marbles. 
# Then, 2 marbles are drawn from the bag, at random, without replacement. 
# If the first marble drawn is red, what is the probability 
# that the second marble is blue?
# # P (2B | 1R) = P (1R and 2B) / P(1R) = 2/7 / 3/7 = 14/21 = 2/3

# Day 5.1
# Binomial distribution 1
# The ratio of boys to girls for babies born in Russia is 1,09:1. 
# If there is 1 child born per birth, what proportion of Russian 
# families with exactly 6 children will have at least 3 boys?
# Write a program to compute the answer using the above parameters. 
# Then print your result, rounded to a scale of 3 decimal places (i.e.,  format)

# Binomial Distribution: Every random experiment is isolate from the others

# f(x)= (n x) * p**x (1-p)**n-x (dbinom)
# p = O(p) / (1 + O(p)) = 1.09 / (1 + 1.09) (0p = y)
# n = 6
# x = 3; x = 4; x= 5; x= 6

arr <- readcommand()

y <- arr[1]
p <- y/(1 + y)
n <- 6

total <- c()
for (x in 3:6){
  total <- c(total, dbinom(x, n, p))
}

totalprob <- round(sum(total), 3)
cat (totalprob)

# Day5.2
# Binomial distribution 2

# A manufacturer of metal pistons finds that, on average, 12%  of the pistons 
# they manufacture are rejected because they are incorrectly sized. 
# What is the probability that a batch of 10 pistons will contain:
# No more than 2 rejects?
# At least 2 rejects?

arr <- readcommand()

p <- arr[1] / 100
n <- arr[2]

total <- c()
for (x in 0:2){
  total <- c(total, dbinom(x, n, p))
}

end1 <- round(sum(total), 3)

total <- c()
for (x in 0:1){
  total <- c(total, dbinom(x, n, p))
}

end2 <- 1 - (round(sum(total), 3))

cat(end1)
cat(end2)

# Statistics Day 5.3
# Geometric distribution 1

# The probability that a machine produces a defective product is 1/3. 
# What is the probability that the 1st defect occurs the 5th item produced?

arr <- readcommand()
p <- (arr[1] / arr[2])

arr <- readcommand()
x <- arr[1] - 1

# Geometric distribution p = probability for event to occurs, x = when occurs
a <- round(dgeom(x, p), 3)
cat(a)

# Alternative version
# Probability of 1st item non defective 2/3 +
# Probability of 2nd item non defective 2/3 +
# Probability of 3rd item non defective 2/3 +
# Probability of 4th item non defective 2/3 +
# Probability of 5th item non defective 1/3

a <- round(2/3 * 2/3 * 2/3 * 2/3 * 1/3, 3)
cat(a)

# Statistics Day 5.3
# Geometric distribution 2

# The probability that a machine produces a defective product is 1/3. 
# What is the probability that the 1st defect is found during the 
# first 5 inspections?

arr <- readcommand()
p <- (arr[1] / arr[2])

arr <- readcommand()
x <- arr[1]

prob <- c()

# Probability sum of defective in the first + non defective in the first + 
# + defective in the second + non defective in the first + non defective in
# the second + defective in the third ... etc... til defective in the fifth
prob <- round(sum(prob, dgeom(0:(x-1), p)), 3)

cat(prob)


# Statistics Day 6.1
# Poisson Distribution I

# A random variable, X , follows Poisson distribution with mean of 2.5. 
# Find the probability with which the random variable X is equal to 5.


arr <- readcommand()
lambda <- (arr[1])

arr <- readcommand()
k <- arr[1]

p <- round(dpois(k, lambda), 3)
cat (p)

# Statistics Day 6.2
# Poisson Distribution II

# The manager of a industrial plant is planning to buy a machine 
# of either type A or type B. For each day’s operation:

# The number of repairs, X, that machine A needs is a Poisson random variable 
# with mean 0.88. The daily cost of operating A is CA:160+40X**2.
# The number of repairs, Y, that machine B needs is a Poisson random variable 
# with mean 1.55. The daily cost of operating B is CB:128+40Y**2.

# Assume that the repairs take a negligible amount of time and the machines 
# are maintained nightly to ensure that they operate like new at the start 
# of each day. 

#Find and print the expected daily cost for each machine.

# X**2 = lambda + lambda **2 => X=sqrt(lambda + lambda **2)

arr <- readcommand()
lambdaA <- (arr[1])
lambdaB <- (arr[2])

costA <- round(160 + (40*((sqrt(lambdaA + (lambdaA**2)))**2)),3)
costB <- round(128 + (40*((sqrt(lambdaB + (lambdaB**2)))**2)),3)
cat(costA)
cat("\n")
cat(costB)

# Statistics Day 6.3
# Normal distribution I

# In a certain plant, the time taken to assemble a car is a random variable, X , 
# having a normal distribution with a mean of 20 hours and a standard deviation 
# of 2 hours. 
# What is the probability that a car can be assembled at this plant in:
# Less than 19.5 hours?
# Between 20 and 22 hours?

arr <- readcommand()
mean <- arr[1]
stdev <- arr[2]

arr <- readcommand()
x <- arr[1]

# Probability less or equal than a point (normal distribution)
prob <- round(pnorm(x, mean, stdev), 3)

arr <- readcommand()
x <- arr[1]
y <- arr[2]

# Probability between two points (normal distribution)
prob2 <- round((pnorm(y, mean, stdev) - pnorm(x, mean, stdev)), 3)

cat(prob)
cat("\n")
cat(prob2)

# Statistics Day 6.4
# Normal distribution II

# The final grades for a Physics exam taken by a large group of students 
# have a mean of 70 and a standard deviation of 10. 
# If we can approximate the distribution of these grades by a normal 
# distribution, what percentage of the students:
# Scored higher than 80 (i.e., have a grade > 80)?
# Passed the test (i.e., have a grade => 60)?
# Failed the test (i.e., have a grade < 60)?
# Find and print the answer to each question on a new line, 
# rounded to a scale of 2 decimal places.

arr <- readcommand()
mean <- arr[1]
stdev <- arr[2]

arr <- readcommand()
x <- arr[1]

# Probability more than a point (normal distribution)
prob <- round(((1 - pnorm(x, mean, stdev))*100), 2)

arr <- readcommand()
x <- arr[1]

# Probability more than a point (normal distribution)
prob2 <- round(((1 - pnorm(x, mean, stdev))*100), 2)

# Probability less or equal than a point (normal distribution)
prob3 <- round((pnorm(x, mean, stdev)*100), 2)

cat(prob)
cat("\n")
cat(prob2)
cat("\n")
cat(prob3)

# Statistics Day 7.1
# The central limit theorem I

# A large elevator can transport a maximum of 9800 pounds. 
# Suppose a load of cargo containing 49 boxes must be transported via the 
# elevator. The box weight of this type of cargo follows a distribution with 
# a mean of 205 pounds and a standard deviation of 15 pounds. 
# Based on this information, what is the probability that all 49 boxes 
# can be safely loaded into the freight elevator and transported?

# normal distribution with:
# (mu = mean * n) and (sigma = stddev *sqrt(n))

arr <- readcommand()
x <- arr[1]

arr <- readcommand()
n <- arr[1]

arr <- readcommand()
mean <- arr[1]

arr <- readcommand()
stddev <- arr[1]

mu <- mean * n
sigma <- stddev * sqrt(n)

prob <- round(pnorm(x, mu, sigma), 4)
cat(prob)


# Statistics Day 7.2
# The central limit theorem II

# The number of tickets purchased by each student for the 
# University X vs. University Y football game follows a distribution 
# that has a mean of 2.4 and a standard deviation of 2.0.
# A few hours before the game starts, 100 eager students line up to purchase 
# last-minute tickets. If there are only 250 tickets left, 
# what is the probability that all 100 students will be able to purchase tickets?

arr <- readcommand()
x <- arr[1]

arr <- readcommand()
n <- arr[1]

arr <- readcommand()
mean <- arr[1]

arr <- readcommand()
stddev <- arr[1]

mu <- mean * n
sigma <- stddev * sqrt(n)

prob <- round(pnorm(x, mu, sigma), 4)
cat(prob)

# Statistics Day 7.3
# The Central Limit Theorem III

# You have a sample of 100 values from a population with mean 500 and with 
# standard deviation 80. 
# Compute the interval that covers the middle 95% of the distribution of 
# the sample mean; 
# in other words, compute A and B such that P(A<x<B). Use the value of z = 1.96. 
# Note that z is the z-score.

arr <- readcommand()
n <- arr[1]

arr <- readcommand()
mean <- arr[1]

arr <- readcommand()
stddev <- arr[1]

arr <- readcommand()
percentage <- arr[1]

arr <- readcommand()
z <- arr[1]

stddev <- stddev / sqrt(n)
upperbound <- ((1 - percentage) / 2) + percentage
lowerbound <- (1 - percentage) / 2

A <- round(qnorm(lowerbound, mean, stddev), 2)
B <- round(qnorm(upperbound, mean, stddev), 2)

cat(A)
cat("\n")
cat(B)

# Statistics Day 8.1
# Pearson Correlation coeficient

# Given two n-element data sets, X and , Y calculate the value of the 
# Pearson correlation coefficient.

arr <- readcommand()
n <- arr[1]

arr <- readcommand()
X <- arr

arr <- readcommand()
Y <- arr

corrcoef <- round(cor(X, Y, method = 'pearson'), 3)
cat(corrcoef)

# Statistics 8.2
# Spearman's Rank Correlation Coefficient

# Given two n-element data sets, X and Y, calculate the value of 
# Spearman's rank correlation coefficient.

arr <- readcommand()
n <- arr[1]

arr <- readcommand()
X <- arr

arr <- readcommand()
Y <- arr

corrcoef <- round(cor(X, Y, method = 'spearman'), 3)
cat(corrcoef)


data <- read.table("stdin",header=F, skip=1)
dat <- t(data)


# Statistics 9.1
# Least square regression line

# A group of five students enrolls in Statistics immediately after 
# taking a Math aptitude test. Each student's Math aptitude test 
# score, x , and Statistics course grade, y , can be expressed as the 
# following list of (x,y) points:
# 1. (95, 85)
# 2. (85, 95)
# 3. (80, 70)
# 4. (70, 65)
# 5. (60, 70)
# If a student scored an 80 on the Math aptitude test, what grade would we 
# expect them to achieve in Statistics? Determine the equation of the 
# best-fit line using the least squares method, then compute and print the 
# value of  y when x = 80.

arr <- readcommand()
X <- c(arr[1])
Y <- c(arr[2])

arr <- readcommand()
X <- c(X, arr[1])
Y <- c(Y, arr[2])

arr <- readcommand()
X <- c(X, arr[1])
Y <- c(Y, arr[2])

arr <- readcommand()
X <- c(X, arr[1])
Y <- c(Y, arr[2])

arr <- readcommand()
X <- c(X, arr[1])
Y <- c(Y, arr[2])

model = lm(Y~X)
print(model)
regressionline = (model$coefficients[2]*80) + model$coefficients[1]
cat(round(ans,3))

# Statistics 9.2
# Pearson Correlation Coefficient II

# The regression line of y on x is 3x + 4y + 8 = 0, and the regression line 
# of y on x is 4x + 3y + 7 = 0. What is the value of the Pearson correlation coefficient?

# -3/4
#



# Statistics 10.1
# Multiple linear Regression

# Andrea has a simple equation:
# Y = a + b1*f1 + b2*f2 + ... + bm*fm
# for (m+1) real constants (a, f1, f2, ... , fm). We can say that the 
# value of Y depends on m features. Andrea studies this equation for  
# n different feature sets (f1, f2, ... , fm) and records each respective 
# value of Y. If she has q new feature sets, can you help Andrea find 
# the value of Y for each of the sets?
# Note: You are not expected to account for bias and variance trade-offs.


arr <- readcommand()
m <- arr[1]
n <- arr[2]

F1 <- c()
F2 <- c()
Y <- c()

for (i in 1:n){
  arr <- readcommand()
  F1 <- c(F1, arr[1])
  F2 <- c(F2, arr[2])
  Y <- c(Y, arr[3])
}

arr <- readcommand()
q <- arr[1]

Fq <- c()
F2q <- c()
for (i in 1:q){
  arr <- readcommand()
  Fq <- c(Fq, arr[1])
  F2q <- c(F2q, arr[2])
}

m <- lm(Y ~ F1 + F2)
show(m)

regressionset <- c()
for (i in 1:q){
  regressionset <- c(regressionset, round(m$coefficients[1] + (m$coefficients[2] * Fq[i]) + (m$coefficients[3] * F2q[i]) , 2))

}

for (i in (1:length(regressionset))){
  cat(regressionset[i])
  cat("\n")
}
