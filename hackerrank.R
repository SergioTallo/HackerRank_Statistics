#HackerRank 10 days Statistic

# Day 1. 1
#Calculate Mean, median, mode

n <- readLines(stdin(), n=1)
n <- as.numeric(n)

a <- readLines(stdin(), n=1)
a <- strsplit(a, " ")
a <- as.numeric(unlist(a))

mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

mean(a)
median(a)
mode(a)

# Day 1. 2
# Calculate Weighted mean

X <- readLines(stdin(), n=1)
X <- strsplit(X, " ")
X <- as.numeric(unlist(X))

W <- readLines(stdin(), n=1)
W <- strsplit(X, " ")
W <- as.numeric(unlist(X))

c <- weighted.mean(X, W)
h <- sprintf(c, fmt = '%#.1f')
cat(h)

# Day 2.1
# Calculate Quartiles

n <- readLines(stdin(), n=1)
n <- as.numeric(n)

a <- readLines(stdin(), n=1)
a <- strsplit(a, " ")
a <- as.numeric(unlist(a))

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

X <- readLines(stdin(), n=1)
X <- strsplit(X, " ")
X <- as.numeric(unlist(X))

W <- readLines(stdin(), n=1)
W <- strsplit(W, " ")
W <- as.numeric(unlist(W))

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

X <- readLines(stdin(), n=1)
X <- strsplit(X, " ")
X <- as.numeric(unlist(X))

error <- c()

for (i in 1:length(X)){
  error <- c(error, (mean(X) - X[i]) ** 2)
}
my_stddev <- round(sqrt(sum(error) / length(X)), 1)



