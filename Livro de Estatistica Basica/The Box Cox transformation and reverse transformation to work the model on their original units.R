##================================================================================================
##                                                                                              
##    Nome: Transformacao de Box-Cox e reverse-transformation                                            
##                                                    
##    site: https://cran.r-project.org/web/packages/bestNormalize/vignettes/bestNormalize.html                                                                                                                                              
##
##
##    Objetivo: mostrar as trasnformacoes possiveis e a volta as unidades originais
##    para "The reverse-transformation will allow us to visualize how these variables work into the model on their original units."
##
##
##    prof. Steven Dutt-Ross                          
##    UNIRIO           
##================================================================================================

# Definição:  we define “normalize” as in “to render data Gaussian”.

# Dois problemas:
 # 1. The (often problematic) assumption of normality of the outcome in the classical linear regression.
 # 2. Applied regression with highly skewed distributions. There exists the tendency to have high leverage points (and highly influential points), even when one centers and scales the covariates. 
  # 2.1 When examining interactions, these influential points can become especially problematic since the leverage of that point is amplified for every child interaction of which it is a parent. 
  # 2.2 Normalization of the covariates mitigates the influence of these covariates, which allows for easier model selection. 
  # 2.3 Popular model selection packages such as caret and recipes have built-in mechanisms to normalize the predictor variables (they call this “preprocessing”). 

# Methods
# The Lambert W x F transformation
# The Box Cox tranformation
# The Yeo-Johnson transformation
# The Ordered Quantile technique


# The autotrader data set  
# I apply the bestNormalize functionality to de-skew mileage, age, and price in my pricing model.
library(bestNormalize)
data("autotrader")
autotrader$yearsold <- 2017 - autotrader$Year

#Anderson-Darling test for the hypothesis of normality
library(nortest)
ad.test(autotrader$yearsold)
ad.test(autotrader$price)
# Não é normal

### Using best-normalize
(priceBN <- bestNormalize(autotrader$price))
(mileageBN <- bestNormalize(autotrader$mileage))
(yearsoldBN <- bestNormalize(autotrader$yearsold))

# mostrando os resultados
# Anderson-Darling normality test
ad.test(priceBN$x.t)
ad.test(autotrader$price)

# Histogramas
par(mfrow = c(3, 2))
MASS::truehist(autotrader$price)
MASS::truehist(priceBN$x.t)
MASS::truehist(autotrader$mileage)
MASS::truehist(mileageBN$x.t)
MASS::truehist(autotrader$yearsold)
MASS::truehist(yearsoldBN$x.t)

# outra forma de mostra a relacao entre X (variavel original) e f(X) (variaavel transformada)
par(mfrow = c(2, 2))
price.xx <- seq(min(autotrader$price), max(autotrader$price), length = 100)
mileage.xx <- seq(min(autotrader$mileage), max(autotrader$mileage), length = 100)
yearsold.xx <- seq(min(autotrader$yearsold), max(autotrader$yearsold), length = 100)
plot(price.xx, predict(priceBN, newdata = price.xx), type = "l", 
     main = "Price bestNormalizing transformation", 
     xlab = "Price ($)", ylab = "g(price)")
plot(mileage.xx, predict(mileageBN, newdata = mileage.xx), type = "l", 
     main = "Mileage bestNormalizing transformation", 
     xlab = "Mileage", ylab = "g(Mileage)")
plot(yearsold.xx, predict(yearsoldBN, newdata = yearsold.xx), type = "l", 
     main = "Years-old bestNormalizing transformation", 
     xlab = "Years-old", ylab = "g(Years-old)")

#----------------------------------------------------------------------------------------
# USO EM UM MODELO DE REGRESSAO
#----------------------------------------------------------------------------------------

# Next, we will fit a linear model on the transformed values of each variable. 
# The reverse-transformation will allow us to visualize how these variables work into the model on their original units.

#For the sake of illustration, we also plot the effects of a GAM model to see if there is a big difference. The GAM models seem to be more influenced by outliers than the fits to the transformed data.

autotrader$price.t <- priceBN$x.t
autotrader$mileage.t <- mileageBN$x.t
autotrader$yearsold.t <- yearsoldBN$x.t

# modelo na variavel transformada (normal)
fit <- lm(price.t ~ mileage.t + yearsold.t,
           data = autotrader)
summary(fit)

miles.t <- predict(mileageBN, newdata = mileage.xx)
c1 <- coef(fit)["mileage.t"]

par(mfrow = c(1, 1))
plot(
  mileageBN$x.t,
  priceBN$x.t,
  pch = 16,
  col = grey(.1, alpha = .2),
  main = "Estimated linear effect (using transformed data)",
  xlab = "g(Mileage)",
  ylab = "g(Price)"
)
lines(miles.t,
      coef(fit)[1] + c1 * miles.t,
      col = "slateblue",
      lwd = 2)
#----------------------------------------------------------------------------------------
# Uso dos modelo em dados transformados (para garantir a normalidade) 
# no banco de dados original (para interpretar os resultados)
# ou seja, ver os efeitos marginas dos regressores depois do resultado significativo)
#----------------------------------------------------------------------------------------

## Mileage effect
plot(
  autotrader$mileage,
  autotrader$price,
  pch = 16,
  col = grey(.1, alpha = .2),
  main = "Mileage effect (re-transformed to original unit)",
  xlab = "Mileage",
  ylab = "Price"
)
line_vals <- miles.t * c1 + coef(fit)[1]
lines(
  mileage.xx,
  y = predict(priceBN, newdata = line_vals, inverse = TRUE),
  lwd = 2,
  col = "slateblue"
)

# Compare to GAM fit
fit_gam <- mgcv::gam(price ~ s(yearsold) + s(mileage), data = autotrader)
p_gam <- predict(fit_gam, newdata = data.frame(yearsold = mean(autotrader$yearsold), 
                                               mileage = mileage.xx))
lines(mileage.xx, p_gam, lwd = 2, col = 'green3')

legend(
  'topright',
  c("GAM fit", "Transformed linear fit"),
  lwd = 2,
  col = c("green3", "slateblue"),
  bty = "n"
)

#----------------------------------------------------------------------------------------

## Years Old effect
yo.t <- predict(yearsoldBN, newdata = yearsold.xx)
c2 <- coef(fit)["yearsold.t"]

plot(
  jitter(autotrader$yearsold, 1.5),
  autotrader$price,
  pch = 16,
  col = grey(.1, alpha = .2), 
  main = "Years old effect (re-transformed to original unit)",
  xlab = "Age (Jittered)",
  ylab = "Price"
)
line_vals <- yo.t * c2 + coef(fit)[1]
lines(
  yearsold.xx,
  y = predict(priceBN, newdata = line_vals, inverse = TRUE),
  lwd = 2,
  col = "slateblue"
)

# Compare to GAM fit
p_gam <- predict(fit_gam, newdata = data.frame(yearsold = yearsold.xx, 
                                               mileage = mean(autotrader$mileage)))
lines(yearsold.xx, p_gam, lwd = 2, col = 'green3')

legend(
  'topright',
  c("GAM fit", "Transformed linear fit"),
  lwd = 2,
  col = c("green3", "slateblue"),
  bty = "n"
)

# Conclusao:
# ou voce usa um modelo não parametrico como o GAM (generalized additive models are the non-parametric functions).
# ou voce faz a transformacao dos dados. 
# Nunca faça o teste t ou o teste F sem verificar o pressuposto de normalidade. 


# GAMs are simply a class of statistical Models in which the usual Linear relationship between the Response and Predictors are replaced by several Non linear smooth functions to model and capture the Non linearities in the data.These are also a flexible and smooth technique which helps us to fit Linear Models which can be either linearly or non linearly dependent on several Predictors Xi to capture Non linear relationships between Response and Predictors
# qgam = quantile non-parametric additive models
#https://cran.r-project.org/web/packages/qgam/vignettes/qgam.html