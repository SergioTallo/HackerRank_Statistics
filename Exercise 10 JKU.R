# 1
# On weekdays between 12:00 and 13:00 one customer enters a specific store on
# average every two minutes. Assuming that customers enter this store independently,
# calculate the probability that exactly two customers enter this store on weekdays
# between 12:55 and 13:00

# Average 1/2 customers per minute. 
# Between 12:55 and 13:00 (5 minutes): 2.5 customers average
# Poisson distribution
# lambda = 2.5  ;  k = 2

lambda <- 2.5
k <- 2

p <- round(dpois(2, 2.5), 4)
print(p)

#Poisson distribution
#p(k)= ((lambda**k)/k!)e**-lambda
palt <- round((((lambda) ** k) / factorial(2)) * exp(1)**(-lambda), 4)
print(palt)


# 2
# A hunter hits his target with only 65% probability. How likely does he score
# more than three hits in exactly ten shots.

# Binomial distribution
# p(x) = binomcoefficient(n,x)*(p**x)*((1-p)**(n-x))
# n = 10
# p = 0.65
# x > 3
# p(1 - p(x<=3))

n <- 10
x <- 3
p <- 0.65
prob <- c()

prob <- c(prob, dbinom((0:x), n, p))

prob <- 1 - round(sum(prob), 4)
print(prob)

#Alternative

probalt <- c()
probalt <- c(probalt, choose(n,(0:x))*(p**(0:x))*((1-p)**(n-(0:x))))

probalt <- round((1 -sum(probalt)), 4)
print(probalt)


# 3
# Someone would like to send Christmas Cards per postal service to 10 friends.
# Out of this 10 friends, 2 live in Vienna. To send the Christmas card, stamps are required
# Unfortunately, the person has only 4 stamps and therefore decides to select 4 of the 10
# friends randomly and to send only these friends a Christmas card. The random variable X
# is the number of friends in Vienna among the 4 selected friends which get a Christmas card
# a) State the probability distribution function of X
# b) Calculate the expected value and variance for X

# Hypergeometric distribution
# f(x) = (binomcoefficient(k,x) * binomcoefficient((n - m),(k - x))) / binomcoefficient(n,k)
# k = 4
# m = 2
# n = 10
# p = M/N = 2/10

#a)

k <- 4
m <- 2
n <- 10

X <- c()
X <- c(X, dhyper(0:2, m, (n - m), k))

print(X)

# Alternative

Y <- c()
Y <- c(Y, (choose(m,0:2) * choose((n - m),(k - 0:2))) / choose(n,k))


# b)

# k = Size of the sample
# p = probability of sample

# E[X] = μ = k * p
# Var(X) = k * p * (1 - p) * (m+n-k)/(m+n-1)

p <- m/n

eX <- k * p
varX <- k * p * (1 - p) * (n-k)/(n-1)
