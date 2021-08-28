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
h <- sprintf(c, fmt = '%#.1f')
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

a <- round(dgeom(x, p), 3)
cat(a)

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
for (i in 1:x){
  prob <- c(prob, dgeom(i, p))
}

a <- round(sum(prob), 3)
cat(a)
