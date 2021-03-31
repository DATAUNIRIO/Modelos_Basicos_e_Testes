library(bestNormalize)

bestNormalize::yeojohnson()




data(mtcars)
names(mtcars)
mtcars$am <- factor(mtcars$am,labels=c('Automatic','Manual'))
modelo1<-lm(hp ~ disp + am ,data = mtcars)
residuos1<-residuals(modelo1)
shapiro.test(residuos1)

# Yeo Johnson Transformation
hP_yeo_johnson <-VGAM::yeo.johnson(mtcars$hp,lambda = 32)
#inverse of Yeo Johnson Transformation
hP_yeo_johnson_inverse <-VGAM::yeo.johnson(hP_yeo_johnson,lambda = 32,inverse = TRUE,derivative = 0)

# Inverse of the Box-Cox transformation
library(car)
modelo2 <- lm(hp ~ disp + am ,data = mtcars)
modelo2.aux <- update(modelo2, boxCoxVariable(hp) ~ .  )
summary(modelo2.aux)

teste <-data.frame(disp =160.0,am="Automatic")
predict(modelo2.aux,teste)

invBoxCox <- function(x, lambda){
    if (lambda == 0) exp(x) else (lambda*x + 1)^(1/lambda)
}
invBoxCox(-133.9177,0.2)

with(mtcars, boxCox(hp ~ disp + am, data = mtcars,
                   lambda = seq(-0.25, 0.5, length = 10)))