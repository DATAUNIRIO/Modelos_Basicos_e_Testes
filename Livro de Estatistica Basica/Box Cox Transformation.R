
# Em uma regressão, transformações podem nos ajudar a alcançar vários objetivos:
# Com transformações, podemos: 1) aproximar a linearidade, 2) obter uma variância constante, e aproximar de uma distribuição normal.
# Esses pressupostos são importantes para o teste de hipóteses e o p-valor sejam válidos.
# Vamos ver dos tipos de transformações: 1) a traformação de Box Cox, 2) a traformação de Yeo-Johnson


# In regression analysis, transformations can help to achieve several goals:
#The first is the goal of approximate linearity
#Transformations can also help to achieve more nearly constant conditional variation,
#Finally, we can consider transformations toward normality, so that the transformed data are as close to normally distributed as possible.


#-------------------------------------------------------------------------------------
#A transformação de Box Cox.
#A transformação de Box Cox é bem flexível. Essa abordagem vai encontrar a melhor de uma família de transformações que deverá aproximar a sua variável o máximo possível de uma distribuição normal. (Box and Cox 1964; Carroll and Ruppert 1981). 


#A Box Cox transformation is more flexible than (but also includes as a special case) the log transformation and will find an appropriate transformation from a family of power transforms that will transform the variable as close as possible to a normal distribution (Box and Cox 1964; Carroll and Ruppert 1981). At the core of the Box Cox transformation is an exponent, lambda (λ), which varies from -5 to 5. All values of λ are considered and the optimal value for the given data is estimated from the training data; The “optimal value” is the one which results in the best transofrmation to an approximate normal distribution. The transformation of the response Y
#has the form:
  
    y(λ)= (Yλ−1)/λ,if λ≠0
            log(Y),if λ=0.

#If your response has negative values, the  transformation is very similar to the Box-Cox but does not require the input variables to be strictly positive. To apply, use step_YeoJohnson(). 

# Box Cox transform a value
lambda <-  forecast::BoxCox.lambda(AirPassengers)
y <- forecast::BoxCox(AirPassengers, lambda)
air.fit <- forecast::Arima(y, order=c(0,1,1),
                           seasonal=list(order=c(0,1,1),period=12), lambda=lambda)
plot(forecast::forecast(air.fit))

# Inverse Box Cox function
aaa<-forecast::InvBoxCox(air.fit[["fitted"]], lambda)
plot(forecast::forecast(aaa))

par(mfrow=c(1,2))
plot(forecast::forecast(aaa))

air.fit <- forecast::Arima(AirPassengers, order=c(0,1,1),
                           seasonal=list(order=c(0,1,1),period=12), lambda=lambda)
plot(forecast::forecast(air.fit))

#--------------------------------------------------------                        
#In order to perform linear regression, there are certain assumptions that need to be considered before building a model. 
#Assumptions regarding residuals, such as normal distribution, constant variance i.e homoscadaticity need to be checked while building a model. However, there will be times when the above assumptions are violated by data. In such instances data transformation can be employed to transform residuals from having a non-normal distribution to a normal distribution. In this post we will look at one such technique called “Box Cox” transformation.

data("mtcars")
names(mtcars)
qqnorm(mtcars$hp)
qqline(mtcars$hp)
shapiro.test(mtcars$hp)

# Escolhendo o lambda
bc<-MASS::boxcox(lm(hp~mpg,data=mtcars),lambda=seq(-2,2,by=.1))
lambda <- bc$x # lambda values
lik <- bc$y # log likelihood values for SSE
bc2 <- cbind(lambda, lik) # combine lambda and lik

sorted_bc <- bc2[order(-lik),] # values are sorted to identify the lambda value for the maximum log likelihood for obtaining minimum SSE
head(sorted_bc, n = 10)

lambda<-sorted_bc[1,1]

mtcars$bc3<-mtcars$hp^sorted_bc[1,1]/sorted_bc[1,1]
mtcars$bc4<-mtcars$hp^((lambda-1)/lambda)

modelo<-lm(hp^sorted_bc[1,1]/sorted_bc[1,1]~mpg,data=mtcars)
shapiro.test(residuals(modelo))

qqnorm(bc3)
qqline(bc3)
shapiro.test(mtcars$bc3)

invlambda<-1/sorted_bc[1,1]

original <- mtcars$hp
after_transformation <- mtcars$bc3
back_transformation <-  mtcars$bc3^invlambda/invlambda

fittedvalues <- data.frame(original, after_transformation, back_transformation)

head(fittedvalues, n = 10)



#Linear transformation can be applied on both predicitors and response variable. Box cox transformation is applied on the response variable.
# https://rpubs.com/bskc/288328

library(recipes)
data(biomass)
rec <- recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
              data = biomass)
yj_trans <- step_YeoJohnson(rec,  all_numeric())

yj_estimates <- prep(yj_trans, training = biomass)

yj_te <- bake(yj_estimates, biomass)

plot(density(biomass_te$sulfur), main = "before")
plot(density(yj_te$sulfur), main = "after")

tidy(yj_trans, number = 1)
tidy(yj_estimates, number = 1)

