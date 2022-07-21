# DDA with R
# Ch 7 work

library(MASS)
library(popbio)
library(vcdExtra)
library(lmtest)
library(vcd)
library(effects)

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

# test whether temperature improves prediction of failure probability
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

exp(cbind(OddsRatio = coef(arth.logistic2), 
          confint(arth.logistic2)))
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

# Example 7.7

binreg_plot(arth.logistic2, type = "link")

# Fig 7.8
binreg_plot(arth.logistic2, type = "link", subset = Sex == "Female",
            main = "Female", xlim = c(25,75), ylim = c(-3,3))
binreg_plot(arth.logistic2, type = "link", subset = Sex == "Male",
            main = "Male", xlim = c(25,75), ylim = c(-3,3))

# Fig 7.9
# probability scale
binreg_plot(arth.logistic2, subset = Sex == "Female",
            main = "Female", xlim = c(25,75))
binreg_plot(arth.logistic2, subset = Sex == "Male",
            main = "Male", xlim = c(25,75))

# 7.3.3 Effect plots

# partial residuals
arth.eff2 <- allEffects(arth.logistic2, partial.residuals = TRUE)
names(arth.eff2)
class(arth.eff2)
arth.eff2[["Sex"]]
arth.eff2[["Sex"]]$model.matrix

# Fig 7.10
# this does not match figure in book
plot(arth.eff2, rows = 1, cols = 3, type = "response", residuals.pch = 15)

plot(arth.eff2, rows = 1, cols = 3, residuals.pch = 15,
     axes=list(y=list(type="rescale", lab="Prob(Better)")))

arth.full <- Effect((c("Age", "Treatment", "Sex")), mod = arth.logistic2)
plot(arth.full)

# Fig 7.11 
# not exactly the same in the book; no box around legend, grid lines
# are different.
plot(arth.full, multiline = TRUE, ci.style = "bands",
     colors = c("red", "blue"), lwd = 3,
     ticks = list(at = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95)),
     key.args = list(x = 0.52, y = 0.92, columns = 1),
     grid = TRUE)

# Fig 7.12
# On probability scale
# not exactly the same in the book; no box around legend, different line types
plot(arth.full, multiline = TRUE, ci.style = "bands",
     type = "response",
     colors = c("red", "blue"), lwd = 3,
     key.args = list(x = 0.52, y = 0.92, columns = 1),
     grid = TRUE)

# 7.4 Case studies

# Example 7.9 Donner party
data("Donner", package = "vcdExtra")
car::some(Donner, 8)

# How survival related to age and sex?
Donner$survived <- factor(Donner$survived, labels = c("no", "yes"))

xtabs(~ family, data = Donner)

# reduce 10 family groups
fam <- Donner$family
levels(fam)[c(3,4,6,7,9)] <- "Other"
fam <- factor(fam, levels = levels(fam)[c(1, 2, 4:6, 3)])
Donner$family <- fam
xtabs(~ family, data = Donner)
xtabs(~ survived + family, data = Donner)

# Fig 7.13
# spineplot
plot(survived ~ family, data = Donner, col = c("pink", "lightblue"))

# Fig 7.14
# generalized pairs plot
library(gpairs)
gpairs(Donner[,c(4,2,3,1)],
       diag.pars = list(fontsize = 20, hist.color = "gray"),
       mosaic.pars = list(gp = shading_Friendly),
       outer.rot = c(45,45))

# Explore relationship of survival to age and sex
# Fig 7.15
gg <- ggplot(Donner, aes(age, as.numeric(survived == "yes"),
                   color = sex)) +
  ylab("Survived") +
  theme_bw() +
  geom_point(position = position_jitter(height = 0.02, width = 0))

gg + geom_smooth(method = "glm", method.args = list(family = binomial), 
              mapping = aes(fill = sex), alpha = 0.2)

# Fig 7.16
# allow quadratic relationship with age
gg + geom_smooth(method = "glm", formula = y ~ poly(x, 2),
                 method.args = list(family = binomial), 
              mapping = aes(fill = sex), alpha = 0.2)
# loess smooth
gg + geom_smooth(method = "loess", span = 0.9,  
                 mapping = aes(fill = sex), alpha = 0.2) +
  coord_cartesian(ylim = c(-0.05, 1.05))

donner.mod1 <- glm(survived ~ age + sex, data = Donner, family = binomial)
car::Anova(donner.mod1)

donner.mod2 <- glm(survived ~ age * sex, data = Donner, family = binomial)
car::Anova(donner.mod2)

donner.mod3 <- glm(survived ~ poly(age, 2) + sex, data = Donner, family = binomial)
donner.mod4 <- glm(survived ~ poly(age, 2) * sex, data = Donner, family = binomial)
car::Anova(donner.mod4)

# compare models
vcdExtra::LRstats(donner.mod1, donner.mod2, donner.mod3, donner.mod4)
# NULL: model fit no different from saturated model

mods <- list(donner.mod1, donner.mod2, donner.mod3, donner.mod4)
LR <- sapply(mods, function(x) x$deviance) # residual deviance
LR <- matrix(LR, nrow = 2, ncol = 2)
rownames(LR) <- c("additive", "non-add")
colnames(LR) <- c("linear", "non-linear")
LR <- cbind(LR, diff = LR[,1] - LR[,2])
LR <- rbind(LR, diff = c(LR[1,1:2] - LR[2,1:2], NA))
LR

library(splines)
donner.mod5 <- glm(survived ~ ns(age, 2) * sex, data = Donner, family = binomial)
car::Anova(donner.mod5)
donner.mod6 <- glm(survived ~ ns(age, 4) * sex, data = Donner, family = binomial)
car::Anova(donner.mod6)

vcdExtra::LRstats(donner.mod4, donner.mod5, donner.mod6)

# effect display
donner.eff6 <- allEffects(donner.mod6, xlevels = list(age = seq(0,50,5)))
plot(donner.eff6, ticks = list(at = c(0.001, 0.01, 0.05, 0.1, 0.25, 0.5,
                                      0.75, 0.9, 0.99, 0.999)))

# Example 7.10 - racial profiling

library(effects)
data("Arrests", package = "carData")
Arrests[sample(nrow(Arrests), 6),]
car::some(Arrests)

Arrests$year <- as.factor(Arrests$year)
summary(Arrests$year)
arrests.mod <- glm(released ~ employed + citizen + checks + 
                     colour*year + colour*age, data = Arrests,
                   family = binomial)
car::Anova(arrests.mod)

lmtest::coeftest(arrests.mod)

# plot main effect of colour
plot(Effect("colour", arrests.mod),
     lwd = 3, ci.style = "bands", main = "",
     xlab = list("Skin color of arrestee", cex = 1.25),
     ylab = list("P(released)", cex = 1.25))

# plot interactions
plot(Effect(c("colour", "age"), arrests.mod),
     lwd = 3, multiline = TRUE, ci.style = "bands", main = "",
     xlab = list("Age", cex = 1.25),
     ylab = list("P(released)", cex = 1.25))

plot(Effect(c("colour", "year"), arrests.mod),
     lwd = 3, multiline = TRUE, ci.style = "bands", main = "",
     xlab = list("Year", cex = 1.25),
     ylab = list("P(released)", cex = 1.25))

arrests.effects <- allEffects(arrests.mod, xlevels = list(age = seq(15,45,5)))
plot(arrests.effects, ci.style = "bands",
     axes=list(y=list(lim=c(-0.9,3))))

# 7.4.2 more complex models

data("ICU", package = "vcdExtra")
names(ICU)
ICU <- ICU[,-c(4,20)]

ICU2 <- ICU[,c("died", "age", "sex", "cancer", "systolic", "admit", "ph", "pco", "uncons")]
saveRDS(ICU2, file = "data/icu.rds")

# fit full model
icu.full <- glm(died ~ ., data = ICU, family = binomial)
summary(icu.full)

LRtest <- function(model){
  c(LRchisq = (model$null.deviance - model$deviance),
    df = (model$df.null - model$df.residual))
}
(LR <- LRtest(icu.full))
pchisq(LR[1], df = LR[2], lower.tail = FALSE)

# update model removing terms whose sign don't make sense
icu.full1 <- update(icu.full, . ~ . - renal - infect)
anova(icu.full1, icu.full, test = "Chisq")

library(rms)
dd <- datadist(ICU[,-1])
options(datadist = "dd")
icu.lrm <- lrm(died ~ ., data = ICU)
icu.lrm1 <- update(icu.lrm, . ~ . - renal - infec)
sum.lrm1 <- summary(icu.lrm1)
plot(sum.lrm1, log = TRUE, main = "odds ratio for died", cex = 1.25,
     col = rgb(0.1, 0.1, 0.8, alpha = c(0.3, 0.5, 0.8)))

# stepwise selection with AIC
library(MASS)
icu.step1 <- stepAIC(icu.full1, trace = FALSE)
icu.step1$anova
# BIC criterion; generally selects smaller models
icu.step2 <- stepAIC(icu.full1, trace = FALSE, k = log(200))
icu.step2$anova

coeftest(icu.step2)

anova(icu.step2, icu.step1, test = "Chisq")

# non-linearity check
icu.glm3 <- update(icu.step2, . ~ . - age + ns(age, 3) +
                     (cancer + admit + uncons)^2)  # all 2-way interactions for binary predictors
anova(icu.step2, icu.glm3, test = "Chisq")

# check for interactions with age
icu.glm4 <- update(icu.step2, . ~. + age * (cancer + admit + uncons))
anova(icu.step2, icu.glm4, test = "Chisq")

# create nomogram
icu.lrm2 <- lrm(died ~ age + cancer + admit + uncons, data = ICU)
plot(nomogram(icu.lrm2), cex.var = 1.2, lplabel = "Log odds death")

levels(ICU$cancer) <- c("-", "Cancer")
levels(ICU$admit) <- c("-", "Emerg")
levels(ICU$uncons) <- c("-", "Uncons")

icu.glm2 <- glm(died ~ age + cancer + admit + uncons, data = ICU, 
                family = binomial)
binreg_plot(icu.glm2, type = "link", conf_level = 0.68,
            legend = FALSE, labels = TRUE, labels_just = c("right", "bottom"),
            cex = 0, point_size = 0.8, pch = 15:17, ylab = "log odds (died)",
            ylim = c(-7,4))

# 7.5 influence and diagnostic plots

# 7.5.1.1 Residuals

# Five different kinds of residuals
# Deviance
residuals(icu.glm4, type = "deviance")[1:5] # default

# Pearson
residuals(icu.glm4, type = "pearson")[1:5] 

# Raw
residuals(icu.glm4, type = "response")[1:5] # raw
# raw residual calculation
(as.numeric(ICU$died[1:5]) - 1) - predict(icu.glm4, type = "response")[1:5]

# Standardized 
rstandard(icu.glm4)[1:5]

# Studentized; preferred in model diagnostics
rstudent(icu.glm4)[1:5]

# 7.5.1.2 Leverage
# measures the potential impact of an individual case on the results
hatvalues(icu.glm4)[1:5]

# Example 7.13: Donner party revisited
infl <- influence.measures(donner.mod3)
summary(infl)

# Fig 7.24
op <- par(mar = c(5, 4, 1, 1) + 0.1, cex.lab = 1.2)
res <- car::influencePlot(donner.mod3, scale = 8,
                          id=list(col = "blue"))
# add vertical line labels
k <- length(coef(donner.mod3))
n <- nrow(Donner)
text(x = c(2,3) * k/n, y = -1.8, c("2k/n", "3k/n"), cex = 1.2)

idx <- which(rownames(Donner) %in% rownames(res))
cbind(Donner[idx,2:4], res)

car::influenceIndexPlot(donner.mod3, vars = c("Cook", "Studentized", "hat"), 
                        id=list(n=4))

# Example 7.14 - ICU revisited
formula(icu.glm2)
library(car)
# Fig 7.26
res <- influencePlot(icu.glm2, scale = 8, 
                     id = list(col = "red", n = 3, cex = 1.5))

idx <- which(rownames(ICU) %in% rownames(res))
cbind(ICU[idx, c("died","age","cancer","admit","uncons")], res)

infIndexPlot(icu.glm2, vars = c("Cook", "Studentized", "hat"),
             id = list(n = 4))

# DFBETA
infl <- influence.measures(icu.glm2)
dfbetas <- data.frame(infl$infmat[,2:5])
colnames(dfbetas) <- c("dfb.age","dfb.cancer","dfb.admit","dfb.uncons")
head(dfbetas)

op <- par(mar = c(5, 5, 1, 1) + 0.1)
cols <- ifelse(ICU$died == "Yes", "red", "blue")
plot(dfbetas[,1], type = "h", col = cols,
     xlab = "Obs index", ylab = expression(Delta * beta[Age]),
     cex.lab = 1.3)
points(dfbetas[,1], col = cols)
big <- abs(dfbetas[,1]) > 0.25
idx <- 1:nrow(dfbetas)
text(idx[big], dfbetas[big,1], label = rownames(dfbetas)[big],
     cex = 0.9, pos = ifelse(dfbetas[big,1] > 0, 3, 1), xpd = TRUE)
abline(h = c(-0.25, 0, 0.25), col = "grey")

scatterplotMatrix(dfbetas, smooth = FALSE, 
                  ellipse = list(levels=c(.95), 
                                 robust=FALSE, fill=FALSE, 
                                 fill.alpha=0.2), 
                  diagonal = list(method ="histogram"),
                  id = list(n = 2), by.groups=TRUE,
                  groups = ICU$died, col = c("blue", "red"))

# 7.5.3 Other diagnostic plots

# Example 7.15 Donner Party again
formula(donner.mod1)
formula(donner.mod3)

# component-plus-residual plots
# check for nonlinear relationship of survival with age
# Fig 7.30
crPlots(donner.mod1, ~age, id=list(n = 2))
# Fig 7.31
crPlots(donner.mod3, ~poly(age, 2), id=list(n = 2))

# added-variable plots (aka, partial regression plots)

# Example 7.16 Donner party
# Fig 7.33
col <- ifelse(Donner$survived == "yes", "blue", "red")
pch <- ifelse(Donner$survived == "yes", 16, 17)
avPlots(donner.mod1, id = list(n = 2), 
        col = col, pch = pch, col.lines = "darkgreen")

# Example 7.17 ICU again

# Fig 7.34
# first, spine plots
# not useful for assessing adequacy of fitted model
op <- par(mfrow = c(2,2), mar = c(4,4,1,2.5) + 0.1, cex.lab = 1.4)
plot(died ~ age, data = ICU, col = c("lightblue", "pink"))
plot(died ~ cancer, data = ICU, col = c("lightblue", "pink"))
plot(died ~ admit, data = ICU, col = c("lightblue", "pink"))
plot(died ~ uncons, data = ICU, col = c("lightblue", "pink"))
par(op)

# Fig 7.35
# added variable plots
pch <- ifelse(ICU$died=="No", 1, 2)
avPlots(icu.glm2, pch = pch, cex.lab = 1.3, id = list(n = 2), 
        col.lines = "red")

icu.glm2a <- glm(died ~ age + cancer + admit + uncons + systolic,
                 data = ICU, family = binomial)
anova(icu.glm2, icu.glm2a, test = "Chisq")
avPlot(icu.glm2a, "systolic", id = list(n = 3), pch = pch, col.lines = "red")
