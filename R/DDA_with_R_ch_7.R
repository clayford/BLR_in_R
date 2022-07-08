# DDA with R
# Ch 7 work

library(MASS)
library(popbio)
library(vcdExtra)
library(lmtest)

# 7.2

p <- c(0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95)
odds <- p/(1 - p)
# logits are symmetric around 0
data.frame(p, odds = as.character(fractions(odds)),
           logit = log(odds))

with(Arthritis, 
     logi.hist.plot(Age, Improved > "None", type = "hist",
                    counts = TRUE, ylabel = "Probability (Better)",
                    xlab = "Age", col.hist = "lightblue"))

# 7.2.1

data("Arthritis")
Arthritis$Better <- as.numeric(Arthritis$Improved > "None")

# fit glm model
arth.logistic <- glm(Better ~ Age, data = Arthritis, family = binomial)
lmtest::coeftest(arth.logistic)

exp(coef(arth.logistic))

# estimated odds of a better response are multiplied by 1.05 for each one year
# increase in age.

# over 10 years, odds are multiplied by 1.64
exp(coef(arth.logistic)["Age"] * 10)

# beta/4 gives a quick estimate of the max effect of x on the probability scale
coef(arth.logistic)["Age"]/4

# linear probability model; not appropriate
arth.lm <- glm(Better ~ Age, data = Arthritis)
coef(arth.lm)["Age"]

# similar to beta/4 estimate from logit model

# 7.2.2

# How much better is the model compared to intercept only (null) model?
# NULL: model fit no different from null model
anova(arth.logistic, test = "Chisq")

# small p-value --> fit better than null model

# How bad is the model compared to the saturated model that fits data perfectly?
# NULL: model fit no different from saturated model
vcdExtra::LRstats(arth.logistic)

# small p-value --> fit not as good as saturated model

# 7.2.3
# create fig 7.4
plot(jitter(Better, .1) ~ Age, data = Arthritis,
     xlim = c(15,85), pch = 16,
     ylab = "Probability (Better)")

# obtain fitted logistic curve
xvalues <- seq(15,85,5)
pred.logistic <- predict(arth.logistic, 
                         newdata = data.frame(Age = xvalues),
                         type = "response", se.fit = TRUE)
# 95% pointwise CI
upper <- pred.logistic$fit + 1.96*pred.logistic$se.fit
lower <- pred.logistic$fit - 1.96*pred.logistic$se.fit

# add CI ribbon
polygon(x = c(xvalues, rev(xvalues)),
        y = c(upper, rev(lower)),
        col = rgb(0, 0, 1, 0.2), 
        border = NA)
lines(xvalues, pred.logistic$fit, lwd = 4, col = "blue")

# add linear probability model fit and smooth lowess curve
abline(arth.lm, lwd = 2)
lines(lowess(Arthritis$Age, Arthritis$Better, f = 0.9),
      col = "red", lwd = 2)


# previous plot with ggplot2
library(ggplot2)
ggplot(Arthritis) +
  aes(x = Age, y = Better) +
  xlim(5, 95) +
  geom_point(position = position_jitter(height = 0.02, width = 0)) +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"),
              alpha = 0.1, fill = "blue", fullrange = TRUE) +
  geom_smooth(method = "lm", se = FALSE, fullrange = TRUE, color = "black") +
  geom_smooth(method = "loess", se = FALSE, span = 0.95, color = "red")

# 7.2.4 grouped binomial data

# event/trials form instead of 0 and 1
# 6 o-rings on each flight
data("SpaceShuttle", package = "vcd")
shuttle.mod <- glm(cbind(nFailures, 6 - nFailures) ~ Temperature, 
                   data = SpaceShuttle, na.action = na.exclude, 
                   family = binomial())
# response as proportion
SpaceShuttle$trials <- 6
shuttle.modw <- glm(nFailures/trials ~ Temperature, weights = trials,
                   data = SpaceShuttle, na.action = na.exclude, 
                   family = binomial())
# two approaches basically indentical
all.equal(coef(shuttle.mod), coef(shuttle.modw))

anova(shuttle.mod, test = "Chisq")

# Fig 7.5
ggplot(SpaceShuttle) +
  aes(x = Temperature, y = nFailures / trials) +
  xlim(30, 81) +
  xlab("Temp (F)") +
  ylab("O-Ring Failure Prob") +
  geom_point(position = position_jitter(width = 0, height = 0.01), size = 2) +
  geom_smooth(mapping = aes(weight = trials), fill = "blue",
              method = "glm", alpha = 0.2, 
              method.args = list(family = binomial), 
              fullrange = TRUE)

# 7.3 multiple logistic regression models

arth.logistic2 <- glm(Better ~ I(Age - 50) + Sex + Treatment, 
                      data = Arthritis, family = binomial)
lmtest::coeftest(arth.logistic2)
# summary(arth.logistic2)

exp(cbind(OddsRatio = coef(arth.logistic2), confint(arth.logistic2)))
# using car package
car::Confint(arth.logistic2, exponentiate=TRUE)

# each year of age multiplies odds of improvement by about 5%
# odds of improvement for males is about 0.23 times the odds of improvement for females.
# odds of improvement for Treated group about 5.8 times higher than the odds of improvement for placebo group

# other car summaries
car::S(arth.logistic2)
car::brief(arth.logistic2)


# 7.3.1 conditional plots
# Figure 7.6
ggplot(Arthritis) +
  aes(x= Age, y = Better, color = Treatment) +
  xlim(5, 95) + 
  theme_bw() +
  geom_point(position = position_jitter(width = 0, height = 0.02)) +
  geom_smooth(mapping = aes(fill = Treatment), 
              method = "glm", method.args = list(family = binomial),
              fullrange = TRUE, alpha = 0.2)

ggplot(Arthritis) +
  aes(x= Age, y = Better, color = Treatment) +
  xlim(5, 95) + 
  theme_bw() +
  geom_point(position = position_jitter(width = 0, height = 0.02)) +
  geom_smooth(mapping = aes(fill = Treatment), 
              method = "glm", method.args = list(family = binomial),
              fullrange = TRUE, alpha = 0.2) +
  facet_wrap(~Sex)

# why the problem in the second plot?
# only one male on placebo felt better
xtabs(~ Sex + Treatment + Better, data = Arthritis)

# 7.3.2 full-model plots
