

#https://github.com/easystats/performance
#remotes::install_github("easystats/performance")

# Model diagnostics
library(performance)
# Check for heteroskedasticity
# Linear models assume constant error variance (homoskedasticity).
# The check_heteroscedasticity() functions assess if this assumption has been violated:
data(cars)
model <- lm(dist ~ speed, data = cars)
check_heteroscedasticity(model)
#> Warning: Heteroscedasticity (non-constant error variance) detected (p = 0.031).




# Check for overdispersion
# Overdispersion occurs when the observed variance in the data is higher than the expected variance from the model assumption (for Poisson, variance roughly equals the mean of an outcome). check_overdispersion() checks if a count model (including mixed models) is overdispersed or not.
library(glmmTMB)
data(Salamanders)
model <- glm(count ~ spp + mined, family = poisson, data = Salamanders)
check_overdispersion(model)
# Overdispersion can be fixed by either modelling the dispersion parameter (not possible with all packages), or by choosing a different distributional family (like Quasi-Poisson, or negative binomial, see (Gelman and Hill 2007)).

# Check for zero-inflation
# Zero-inflation (in (Quasi-)Poisson models) is indicated when the amount of observed zeros is larger than the amount of predicted zeros, so the model is underfitting zeros. In such cases, it is recommended to use negative binomial or zero-inflated models.
# Use check_zeroinflation() to check if zero-inflation is present in the fitted model.

model <- glm(count ~ spp + mined, family = poisson, data = Salamanders)
check_zeroinflation(model)

# Check for singular model fits
# A “singular” model fit means that some dimensions of the variance-covariance matrix have been estimated as exactly zero. This often occurs for mixed models with overly complex random effects structures.
# check_singularity() checks mixed models (of class lme, merMod, glmmTMB or MixMod) for singularity, and returns TRUE if the model fit is singular.

library(lme4)
data(sleepstudy)

# prepare data
set.seed(123)
sleepstudy$mygrp <- sample(1:5, size = 180, replace = TRUE)
sleepstudy$mysubgrp <- NA
for (i in 1:5) {
  filter_group <- sleepstudy$mygrp == i
  sleepstudy$mysubgrp[filter_group] <- sample(1:30, size = sum(filter_group), replace = TRUE)
}

# fit strange model
model <- lmer(Reaction ~ Days + (1 | mygrp/mysubgrp) + (1 | Subject), data = sleepstudy)

check_singularity(model)
#Remedies to cure issues with singular fits can be found here.



# Comprehensive visualization of model checks
#performance provides many functions to check model assumptions, like check_collinearity(), check_normality() or check_heteroscedasticity(). To get a comprehensive check, use check_model().
# defining a model
model <- lm(mpg ~ wt + am + gear + vs * cyl, data = mtcars)
# checking model assumptions
check_model(model)
