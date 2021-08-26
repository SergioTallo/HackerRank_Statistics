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

