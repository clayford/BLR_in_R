# car::Anova LR Chisq calculations
tm1 <- glm(died ~ sex + uncons, data = icu, family = binomial)
tm2 <- glm(died ~ sex + uncons + age, data = icu, family = binomial)

tm1$deviance - tm2$deviance
-2*(logLik(tm1) - logLik(tm2))


