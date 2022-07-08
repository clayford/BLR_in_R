
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



# intercept only ----------------------------------------------------------


d <- rbinom(n = 100, size = 1, prob = 0.4)
mean(d)
table(d) |> proportions()
prop.test(x = 37, n = 100)

m0 <- lm(d ~ 1)
summary(m0)
m <- glm(d ~ 1, family = binomial)
summary(m)
confint(m)

plogis(coef(m))
plogis(confint(m))



# SoreThroat --------------------------------------------------------------


st <- read.table("data/SoreThroat.dat", header = TRUE)
names(st) <- c("duration", "device", "sore")
st$device <- factor(st$device, labels = c("mask", "tube"))
st$soreF <- factor(st$sore, labels = c("no", "yes"))
summary(st$duration)
summary(st$device)
summary(st$soreF)

xtabs(~ soreF, data = st) |> 
  proportions()
xtabs(~ device + soreF, data = st) |> 
  proportions(margin = 1) |> round(2)  # 0.78 vs 0.47

# 0.78 vs 0.47: is that difference "significant"?
prop.test(x = c(14, 8), n = c(18, 17), correct = FALSE)

# the probability of seeing a difference bigger than that if there truly is no
# difference is about 0.06.

lrm1 <- glm(soreF ~ device, data = st, family = binomial)
summary(lrm1)
predict(lrm1, newdata = data.frame(device = c("mask", "tube")), 
        type = "response")

stripchart(duration ~ soreF, data = st, method = "jitter")

library(ggplot2)
ggplot(st) +
  aes(x = duration, y = soreF, color = device) +
  geom_jitter(width = 0, height = 0.1)

m0 <- glm(sore ~ 1, data = st, family = binomial)
summary(m0)
predict(m0, type = "response")

m <- glm(sore ~ duration + device, data = st, family = binomial)
summary(m)
exp(coef(m))

m2 <- glm(sore ~ duration + device + duration:device, 
         data = st, family = binomial)
summary(m2)

library(ggeffects)
plot(ggpredict(m, terms = c("duration", "device")))
plot(ggpredict(m2, terms = c("duration", "device")))



# logit -------------------------------------------------------------------

lm1 <- lm(sore ~ duration, data = st)
summary(lm1)
head(predict(lm1)) # predicted value greater than 1 at obs 5

# would like predictions in range of 0-1
prob <- seq(0.001, 0.999, 0.001)
prob

# logit (log odds)
odds <- prob/(1 - prob)
logodds <- log(odds)
summary(prob)  # ranges 0 - 1
summary(logodds) # ranges -Inf - +Inf

# qlogis() takes log odds
summary(qlogis(prob))

# logistic regression returns results on the log odds scale
glm1 <- glm(sore ~ duration, data = st, family = binomial)
summary(glm1)
head(predict(glm1))

# need to take inverse of log odds to get probability
# plogis is inverse logit
plogis(head(predict(glm1)))
head(predict(glm1, type = "response"))

qlogis(plogis(head(predict(glm1))))


placekick <- read.csv('../../jcf2d/OneDrive - University of Virginia/_Statistics/ACD_with_R/all_programs_data/Chapter2/Placekick.csv')

# Find the observed proportion of successes at each distance
w <- aggregate(good ~ distance, data = placekick, FUN = sum)
n <- aggregate(good ~ distance, data = placekick, FUN = length)
placekick2 <- data.frame(distance = w$distance, 
                         success = w$good, 
                         trials = n$good, 
                         proportion = round(w$good/n$good,4))
head(w.n)
tail(w.n)

mod.fit.bin<-glm(formula = success/trials ~ distance, weights = trials, family = binomial(link = logit), data = w.n, trace = TRUE)
summary(mod.fit.bin) 