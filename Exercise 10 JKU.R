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

p <- round(dpois(k, lambda), 4)
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


# 4
# The body height of a giraffe species is normally distributed with a mean of 4.5m 
# and a standard deviation of 2.1m
# Calculate the limits of an interval, which covers the central 95% of the distribution.
# The "central 95%" of a distribution are distributed symmetrically around the expected value.

mean <- 4.5
stddev <- 2.1
percentage <- 0.95
upperbound <- 1 - ((1 -percentage)/2)
lowerbound <- (1 -percentage)/2

uplimit <- qnorm(upperbound, mean, stddev)
downlimit <- qnorm(lowerbound, mean, stddev)

print(uplimit)
print(downlimit)

# Alternative

# z1 = mean - (d - mean) / stddev => d = -(z1 * stddev)
# z2 = mean + (d - mean) / stddev => d = z2 * stddev

z1 <- qnorm(lowerbound)
z2 <- qnorm(upperbound)

d <- z2 * stddev

downlimit2 <- mean - d
uplimit2 <- mean + d 

print(uplimit2)
print(downlimit2)

# 5
# At a busy interseccion (100.000 cars per week) on average, seven traffic accidents
# per week happen. What is the approximative probability for at most one accident
# to occur in a week?

# Poisson distribution
# lambda = 7
# n = 100000
# Every week 7 accidents

lambda <- 7
k <- 1

p <- c()
p <- c(p, dpois(0:1, lambda))
p <- round(sum(p), 4) * 100
print(p)

# 6
# The length of a workpiece (in cm) is distributed approximately normal with
# mean = 6 and variance = 0.16
# a) How likely is it that such a workpiece is shorter than 6.4 cm?
# b) How likely is it that such a workpiece is longer than 6.4 cm?
# c) How likely is it that such a workpiece is exactly 6.4 cm?

# a)
mean <- 6
variance <- 0.16
stddev <- sqrt(variance)
x <- 6.4

prob <- round(pnorm(x, mean, stddev), 4)

# Alternative

z <- ((x - mean) / stddev)
prob <-pnorm(z)


# b)

prob <- 1 - prob


# c)
# The probability is 0%


# 7
# A machine automatically fills coffee packs. Based on long observations, it is known
# that the filled coffee quantity is normally distributed with a mean of 498 grams and
# a standard deviation of 7 grams. Due to new regulations, coffee packs with 500 grams
# and a tolerance of 5 grams are now required. What is the proportion of coffee packs that 
# do not meet the requirements?

mean <- 498
stddev <- 7
x1 <- 495
x2 <- 505

# P(x < 495 and x > 505) = 1 - P(495 < x < 505)
# P(495 < x < 505) = P(x < 505) - P(x < 495)

probx1 <- pnorm(x1, mean, stddev)
probx2 <- pnorm(x2, mean, stddev)

prob <- round((1 - (probx2 - probx1)), 4) * 100
print(prob)

# Alternative

z1 <- ((x1 - mean) / stddev)
probx1 <-pnorm(z1)

z2 <- ((x2 - mean) / stddev)
probx2 <-pnorm(z2)

prob <- round((1 - (probx2 - probx1)), 4) * 100
print(prob)


# 8
# The survival time of a certain type of LED lamps is normally distributed with the
# mean μ = 1000 hours and the variance of ∂**2= 10000 hours**2. 
# Calculate the probability that a randomly selected LED lamp will survive for less than 1200 hours.

mean <- 1000
variance <- 10000
stddev <- sqrt(variance)
x <- 1200

prob <- round(pnorm(x, mean, stddev)*100, 2)
print(prob)

# Alternative

z <- ((x - mean) / stddev)
prob <- round(pnorm(z)*100, 2)
print(prob)


# 9
# A company receives a shipment of 100 specific parts. The delivery conditions allow a maximum
# of 5% defective parts in a shipment. In the case of 5% more defective parts the shipment is 
# returned at the supplier's expense.
# a) What is the exact probability that a shipment that is still ok, i.e. a shipment with exactly
#   5% defective parts is returned, if the company uses the following quality test plan:
#       12 units of the shipment are randomly selected without replacement and checked. If there
#       is more than one defective unit among this 12 checked units, the shipment will be returned
#       without further verification.
# b) What is the result using an appropriate approximative solution? State the name of your approximation.

# a)

n <- 100
m <- 5 # The shipment have a 5% of defective parts
k <- 12

X <- c()
X <- c(X, dhyper(0:1, m, (n - m), k))
X <- sum(X)
prob <- round((1 - X) * 100, 2)
print(prob)

# b)
p <- 0.05
n <- 100
x <- 1

X <- c()
X <- c(X, dbinom(0:1, n, p))
X <- sum(X)
print(X)


# 10
# The probability that a person will not tolerate a certain medication is 0.002.
# A total of 2000 patients were treated by this medication.
# a) What is the approximate probability that at most 2 patients do not tolerate the medication?
# b) What is the approximate probability that at least 2 patients do not tolerate the medication?

# Poisson distribution (because approximate)

# a)

p <- 0.002
n <- 2000
lambda <- n * p

prob <- c()
prob <- c(prob, dpois(0:2, lambda))
prob <- round(sum(prob), 4) * 100
print(prob)

# b)

prob <- c()
prob <- c(prob, dpois(0:1, lambda))
prob <- round(( 1 - sum(prob)), 4) * 100
print(prob)

# 11
# Judith owns a bar. The daily beer consumption measured in liters of Judith's customers is
# considered a normally distributed random variable. This random variable is defined as
# X~N(300, 6900). Which quantity of beer does Judith have to stock up, so that there is a 95%
# chance the beer will be enough tomorrow, assuming she has run out of beer today?

µ <- 300
variance <- 6900
stddev <- sqrt(variance)
upperbound <- 0.95

uplimit <- qnorm(upperbound, µ, stddev)

print(uplimit)

# Alternative

z <- qnorm(upperbound)

d <- z * stddev

uplimit2 <- µ + d 

print(uplimit2)


# 12
# 100 pieces of a specific product are produced. One piece is defective with a probability of 5%.
# The pieces are produced independent of each other. Caculate the probability for at most
# two defective pieces among these 100 pieces.

# Binomial distribution
# p(x) = binomcoefficient(n,x)*(p**x)*((1-p)**(n-x))
# n = 100
# p = 005
# x ≤ 3
# p(x≤2)

n <- 100
x <- 2
p <- 0.05
prob <- c()

prob <- c(prob, dbinom((0:x), n, p))

prob <- round(sum(prob) * 100, 2)
print(prob)

#Alternative

probalt <- c()
probalt <- c(probalt, choose(n,(0:x))*(p**(0:x))*((1-p)**(n-(0:x))))

probalt <- round((sum(probalt) * 100), 2)
print(probalt)

# 13
# The weight of coffee bags is normally distributed with µ = 246g . 10% of these bags weigh less
# than 237g. Calculate the probability that...
# a) One bag weighs more than 247g
# b) Under 10 bags at least 8 bags weigh less than 247g.

µ <- 246
percentage <- 0.1
x <- 237

z <- qnorm(percentage)
stddev <- (x-µ) / z

# a)

x <- 247

prob <- round((1 - pnorm(x, µ, stddev)) * 100, 2)
print(prob)

# b)
# Binomial distribution. With a probability of 44,34% from 10 bags not more than 2 over 247g.

p <- prob / 100
x <- 2
n <- 10

prob <- c()
prob <- c(prob, dbinom((0:x), n, p))
prob <- round(sum(prob) * 100, 2)
print(prob)


# 14
# On average 6% of all passengers who reserved seats do not show up for their booked flight. 
# The airline Econair knows this and reserves 230 flight tickets for 220 available seats. 
# What are the chances that all passengers who actually come to a flight get a seat? 
# (It can be assumed that all the passengers make their decisions independently from each other)
# a) Calculate the exact probability
# b) Calculate an asymtotic solution using Poisson distribution
# c) Calculate an asymtotic solution using the normal distribution

# a)
# Binomial distribution

p <- 0.06
n <- 230
x <- 9
# Probability that at most 9 passengers don't come

prob <- c()
prob <- c(prob, dbinom((0:x), n, p))
prob <- sum(prob)

# Because we are searching that at most 220 attend we need 1 - prob

prob <- round((1 - prob) * 100, 2)
print(prob)

# b)
# Poisson distribution

lambda <- n * p

prob <- c()
prob <- c(prob, dpois(0:x, lambda))
prob <- round(( 1 - sum(prob)), 4) * 100
print(prob)

# c)
# z = (y - (n * p)) / sqr((n * p) * (1 - p))

y <- x

z <- (y - (n * p)) / sqrt((n * p) * (1 - p))
prob <- round((1 - pnorm(z)) * 100, 2)
print(prob)


# 15
# An airline knows that the body weight of passengers is normally distributed with µ = 80kg
# and stddev = 100kg**2. A specific plane can carry 154 passengers.
# a) Calculate the expected value and variance for the total weight of the passengers of a fully
# occupied plane of this type.
# b) Calculate the probability that the total weight of the passengers of a fully occupied plane
# of this type is greater than 12100kg.

# a)

# E(s) = n * µ
# var = n * stddev

µ <- 80
n <- 154
stddev <- 100

expected <- n * µ
variance <- n * stddev
print(expected)
print(variance)

# b)

x <- 12100
stddev <- sqrt(variance)
µ <- expected

# 1 - prob(x < 12100)

prob <- round((1 - pnorm(x, µ, stddev)) * 100, 2)
print(prob)

