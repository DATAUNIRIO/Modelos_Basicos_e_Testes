
# https://easystats.github.io/performance/reference/index.html



library(performance)
model <- lm(mpg ~ wt + cyl, data = mtcars)
r2(model)

model <- glm(am ~ wt + cyl, data = mtcars, family = binomial)
r2(model)

library(MASS)
data(housing)
model <- polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
r2(model)

data(cars)
model <- lm(dist ~ speed, data = cars)
check_heteroscedasticity(model)

# defining a model
model <- lm(mpg ~ wt + am + gear + vs * cyl, data = mtcars)
# checking model assumptions
check_model(model)
check_model(model, panel = FALSE)

m <<- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
check_normality(m)
#plot(m)

plot(check_normality(m), type = "qq")
plot(check_normality(m), type = "pp")






