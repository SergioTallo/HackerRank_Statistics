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
# Out of this 10 friends, 2 live in Vienna,
