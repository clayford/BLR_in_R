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


set.seed(125)
dat <- lme4::InstEval[sample(nrow(lme4::InstEval), 800), ]
fit <- glm(y > 3 ~ lectage + studage + service + dept, binomial, dat)
pdat <- with(dat, data.frame(y = ifelse(y > 3, 1, 0),
                             prob = predict(fit, type = "response")))

apparent.cal <- data.frame(with(pdat, lowess(prob, y, iter = 0)))

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
srange <- seq(min(pdat$prob), max(pdat$prob), length.out = 60)

## Discard the smallest and largest probs for robustness, and agreement with rms::calibrate
srange <- srange[5 : (length(srange) - 4)]

## The apparent calibration is determined by this loess curve.
apparent.cal.fit <- with(pdat, lowess(prob, y, iter = 0))
app.cal.fun <- approxfun(apparent.cal.fit$x, apparent.cal.fit$y)
app.cal.pred <- app.cal.fun(srange)

## Number of bootstrap replicates
nsim <- 300

## Storage for bootstrap optimism (one row per bootstrap resample)
opt.out <- matrix(NA, nsim, length(srange))

i <- 1
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
  
  ## Apply the bootstrap model to the original data
  prob.boot.orig <- predict(fit.boot, dat, type = "response")
  
  ## Make a DF of the boot model predictions on original data
  pdat.boot.orig <- data.frame(y = ifelse(dat$y > 3, 1, 0),
                               prob = prob.boot.orig)
  
  ## Fit a calibration curve to the original data w/ boot model predictions
  boot.cal.orig.fit <- with(pdat.boot.orig, lowess(prob, y, iter = 0))
  boot.cal.orig.fun <- approxfun(boot.cal.orig.fit$x, boot.cal.orig.fit$y, 
                                 ties = "ordered")
  
  ## Collect a set of them for comparison
  boot.cal.orig.pred <- boot.cal.orig.fun(srange)
  
  ## Take the difference for estimate of optimism
  opt <- boot.cal.pred - boot.cal.orig.pred
  opt.out[i, ] <- opt
}
