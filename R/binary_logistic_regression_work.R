
# simulate binary data using binomial distribution
# rbinom: random data from a binomial distribution with size and p
# size and p are the "parameters"

# simulate flipping 1 fair coin (n = 1), 1 time (size = 1)
# Let's 0 = tails, 1 = heads
rbinom(n = 1, size = 1, prob = 0.5)

# simulate flipping 1 fair coin (n = 1), 5 times (size = 5)
# number of heads is returned
rbinom(n = 1, size = 5, prob = 0.5)

# simulate flipping 50 fair coins (n = 5), 1 time each (size = 1)
rbinom(n = 50, size = 1, prob = 0.5)

# can estimate p (work backwards) taking mean of 0s and 1s
flips <- rbinom(n = 50, size = 1, prob = 0.5)
mean(flips)

# Above the probability is the same for each flip.

# Another example: scoring in basketball
# either make the shot or you don't.

# simulate taking 10 free throws (n = 10) with p = 0.8
rbinom(n = 10, size = 1, prob = 0.8)

# But what if we take distance into account? There's higher probability scoring
# closer to the basket than farther out. So p changes with distance.

# Assume probability from under basket (0) is 0.99, and decreases by 0.02 for
# each additional foot from the basket
d <- 0:40
0.99 - 0.02 * d

# simulate 10000 shots with these probabilities
n <- 10000
distance <- sample(x = 0:40, size = n, replace = TRUE)
basket <- rbinom(n = n, size = 1, prob = 0.99 - 0.02 * distance)

dat <- data.frame(distance, basket)

# estimate probability given distance; work backwards
# mean of 0 and 1 is proportion of 1s
aggregate(basket ~ distance, data = dat, mean)

# But what if we think probability decreases by 0.03 for each foot farther from
# basket?
d <- 0:40
0.99 - 0.03 * d

# We get negative probabilities! We can't think in terms of absolute changes in
# probability. Instead we need to think in terms of odds ratios.

# probability
0.75
0.78

# odds
0.75/(1 - 0.75)
0.78/(1 - 0.78)

# odds ratio
(0.78/(1 - 0.78)) / (0.75/(1 - 0.75))  # odds increase by 1.18 (18%)
(0.75/(1 - 0.75)) / (0.78/(1 - 0.78))  # odds decrease by 0.85 (15%)

# So instead of thinking probability decreases by 0.03 for every 1 foot, we
# think of the odds ratio decreasing/increasing by, say, 0.85 for every 1 foot.

# Furthermore we can take the log of the odds ratio and get log-odds
log( (0.75/(1 - 0.75)) / (0.78/(1 - 0.78)) )

# log odds converts p from [0,1] to (-Inf, Inf)

# To convert log odds to probability, we use the logistic function: plogis()
plogis(-0.16)

# To convert probability to log odds, we use the inverse logistic function:
# qlogis()
qlogis(0.46)

# returning to our basketball example, when d = 0, probability is 0.99. Convert
# to log odds:
qlogis(0.99)

# And say we expect the odds ratio to decrease by 11%, or 0.89, for each
# additional foot of distance.
log(0.89)

# to convert these to probabilities, for distance 10 and 15 feet
plogis(4.5 + -.12 * c(10, 15))

# Now redo do our simulation

# simulate 10000 shots with these probabilities
n <- 10000
distance <- sample(x = 0:40, size = n, replace = TRUE)
basket <- rbinom(n = n, size = 1, 
                 prob = plogis(4.5 - 0.12 * distance))
dat <- data.frame(distance, basket)

# estimate probability given distance; work backwards
# mean of 0 and 1 is proportion of 1s
aggregate(basket ~ distance, data = dat, mean)

# Using logistic regression
m <- glm(basket ~ distance, data = dat, family = binomial)
summary(m)

# odds ratio
exp(coef(m))

# prediction at d = 10, 11
p <- predict(m, newdata = data.frame(distance = c(10, 11)), 
             type = "response")
# probabilities
p
# odds
p[1]/(1 - p[1]) # d = 10
p[2]/(1 - p[2]) # d = 11

# odds ratio
(p[2]/(1-p[2]))/(p[1]/(1- p[1]))

# compare
exp(coef(m)["distance"])

# plot predicted probability
library(ggeffects)
plot(ggpredict(m, terms = "distance[all]"))
