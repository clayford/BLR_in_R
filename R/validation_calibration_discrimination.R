# Validation

# Whether predicted values from the model are likely to accurately predict
# responses on future subjects or subjects not used to develop our model. Can
# help ensure model was not overfitted.

# Two major aspects of predictive accuracy

# 1. Calibration: ability of the model to make unbiased estimates of outcome
# (aka reliability)

# 2. Discrimination: model's ability to separate subjects' outcomes



library(bootstrap)
data("hormone")
str(hormone)
mh <- lm(amount ~ -1 + hrs + Lot, data = hormone)
summary(mh)
sum(resid(mh)^2)/27  # book rse
sum(resid(mh)^2)/23
sigma(mh)^2
sigmaHat(mh)
sigma(mh)

# p. 248 example in Intro to Bootstrap
rse <- function(x)sum(resid(x)^2)/length(x$residuals)
avg_optimism <- function(x){
  fit <- predict(x, newdata = hormone)
  rse1 <- sum((hormone$amount - fit)^2)/27
  rse2 <- sum(resid(x)^2)/27
  rse1 - rse2
  }
boot.rse <- car::Boot(mh, f = avg_optimism, R = 999)
mean(boot.rse$t) # average optimism
boot.rse
library(boot)
plot(boot.rse)

# logistic regression
URL <- "https://github.com/clayford/BLR_in_R/raw/main/data/icu.rds"
icu <- readRDS(file = url(URL))

m <- glm(died ~ age + uncons + admit, data = icu, family = binomial)
summary(m)
LP <- predict(m)

# Harrell p. 259
# gamma = 0,1
mc <- glm(died ~ LP, data = icu, family = binomial)
coef(mc)

# Harrell p. 115
# slope and intercept calibration

i <- sample(seq(nrow(icu)), replace = TRUE)
icu.resample <- model.matrix( ~ age + uncons + admit, data = icu[i,])
icu.resample %*% coef(m)

gamma <- function(d, index){
  dat <- d[index,]
  X <- model.matrix( ~ age + uncons + admit, 
                                data = dat)
  lp <- X %*% coef(m)
  mc <- glm(died ~ lp, family = binomial, data = dat)
  coef(mc)
}
gamma.boot <- boot(data = icu, statistic = gamma, R = 999)
gamma.boot
c(0,1) - apply(gamma.boot$t,2,mean)

library(rms)
m_rms <- lrm(died ~ age + uncons + admit, data = icu, x = TRUE, y = TRUE)
v <- validate(m_rms, B = 200)
v

plot(calibrate(m_rms, B = 400))
grid()
# https://randomeffect.net/post/2021/03/10/bias-corrected-calibration-curve-from-scratch/


library(ggplot2); theme_set(theme_bw(base_size = 14))
library(rms)
set.seed(125)
dat <- lme4::InstEval[sample(nrow(lme4::InstEval), 800), ]
fit <- glm(y > 3 ~ lectage + studage + service + dept, binomial, dat)
pdat <- with(dat, data.frame(y = ifelse(y > 3, 1, 0),
                             prob = predict(fit, type = "response")))

apparent.cal <- data.frame(with(pdat, lowess(prob, y, iter = 0)))

apparent.cal2 <- with(pdat, lowess(prob, y, iter = 0))
apparent.cal2.fun <- approxfun(apparent.cal2$x, apparent.cal2$y, 
                               ties = "ordered")

apparent.cal2.fun(0.4)
apparent.cal2.fun(0.3)

p <- ggplot(pdat, aes(prob, y)) +
  geom_point(shape = 21, size = 2) +
  geom_abline(slope = 1, intercept = 0) +
  geom_line(data = apparent.cal, aes(x, y), linetype = "dotted") +
  scale_x_continuous(breaks = seq(0, 1, 0.1)) +
  scale_y_continuous(breaks = seq(0, 1, 0.1)) +
  xlab("Estimated Prob.") +
  ylab("Data w/ Empirical Prob.") +
  ggtitle("Logistic Regression Calibration Plot")
print(p)

## Range of inputs on which to calculate calibrations
# pdat$prob are predicted probabilities from model
srange <- seq(min(pdat$prob), max(pdat$prob), length.out = 60)

## Discard the smallest and largest probs for robustness, and agreement with rms::calibrate
srange <- srange[5 : (length(srange) - 4)]

## The apparent calibration is determined by this loess curve.
apparent.cal.fit <- with(pdat, lowess(prob, y, iter = 0))
app.cal.fun <- approxfun(apparent.cal.fit$x, apparent.cal.fit$y, 
                         ties = "ordered")
app.cal.pred <- app.cal.fun(srange)

## Number of bootstrap replicates
nsim <- 300

## Storage for bootstrap optimism (one row per bootstrap resample)
# dim = 300 x 52
opt.out <- matrix(NA, nsim, length(srange))

# i <- 1
for (i in 1 : nsim) {
  
  ## Sample bootstrap data set from original data
  dat.boot <- dat[sample(nrow(dat), nrow(dat), TRUE), ]
  
  ## Fit logistic model using the bootstrap data
  fit.boot <- update(fit, data = dat.boot)
  
  ## Make a DF of the bootstrap model and bootstrap predictions
  pdat.boot <- data.frame(y = ifelse(dat.boot$y > 3, 1, 0),
                          prob = predict(fit.boot, dat.boot, type = "response"))
  
  ## Fit a calibration curve to the bootstrap data
  boot.cal.fit <- with(pdat.boot, lowess(prob, y, iter = 0))
  boot.cal.fun <- approxfun(boot.cal.fit$x, boot.cal.fit$y, ties = "ordered")
  
  ## Collect a set of them for comparison
  boot.cal.pred <- boot.cal.fun(srange)
  
  ## Make a DF of the boot model predictions on original data
  pdat.boot.orig <- data.frame(y = ifelse(dat$y > 3, 1, 0),
                               prob = predict(fit.boot, dat, type = "response"))
  
  ## Fit a calibration curve to the original data w/ boot model predictions
  boot.cal.orig.fit <- with(pdat.boot.orig, lowess(prob, y, iter = 0))
  boot.cal.orig.fun <- approxfun(boot.cal.orig.fit$x, boot.cal.orig.fit$y, 
                                 ties = "ordered")
  
  ## Collect a set of them for comparison
  boot.cal.orig.pred <- boot.cal.orig.fun(srange)
  
  ## Take the difference for estimate of optimism
  ## boot model on boot data - boot model on original data
  opt <- boot.cal.pred - boot.cal.orig.pred
  opt.out[i, ] <- opt
}

## The bias corrected calibration curve is the apparent calibration less the average bootstrap optimism
bias.corrected.cal <- app.cal.pred - colMeans(opt.out)

ppdat <- data.frame(srange, app.cal.pred, bias.corrected.cal)

ggplot(ppdat, aes(srange, app.cal.pred)) +
  geom_line(linetype = "dotted", color = "black") +
  geom_line(aes(y = bias.corrected.cal), color = "black") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  # scale_x_continuous(breaks = seq(0, 1, 0.1)) +
  # scale_y_continuous(breaks = seq(0, 1, 0.1)) +
  xlab("Estimated Prob.") +
  ylab("Empirical Prob.") +
  ggtitle("Logistic Regression Calibration Plot")

refit <- lrm(y > 3 ~ lectage + studage + service + dept, dat, x = TRUE, y = TRUE)
val <- validate(refit, B = 200)
val
cal <- calibrate(refit, B = 200)
plot(cal)

# example of perfect calibration curve
set.seed(1)
n <- 1000
x1 <- sample(0:1, size = n, replace = TRUE)
x2 <- round(runif(n = n, min = 1, max = 5), 2)
x3 <- round(rnorm(n = n), 2)
lp <- -3.5 + 1.3*x1 + 0.9*x2 + -0.3*x3 + 0.2*x1*x2 
p <- plogis(lp)
summary(p)
hist(p)
y <- rbinom(n = n, size = 1, prob = p)
table(y)
d <- data.frame(y, x1, x2, x3)

# correct model
m <- glm(y ~ x1 + x2 + x3 + x1:x2, binomial, data = d)
summary(m)

pdat <- data.frame(phat = predict(m, type = "response"),
                   y = y)
ggplot(pdat) +
  aes(x = phat, y = y) +
  geom_point(alpha = 1/5) +
  geom_smooth(method = 'loess', se = F) +
  geom_abline(intercept = 0, slope = 1, color = "red")

# example of not-so-perfect perfect calibration curve
# incorrect model
m2 <- glm(y ~ x3, binomial, data = d)
summary(m2)

pdat2 <- data.frame(phat = predict(m2, type = "response"),
                   y = d$y)
ggplot(pdat2) +
  aes(x = phat, y = y) +
  geom_point(alpha = 1/5) +
  geom_smooth(method = 'loess', se = F) +
  geom_abline(intercept = 0, slope = 1, color = "red")


