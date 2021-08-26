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