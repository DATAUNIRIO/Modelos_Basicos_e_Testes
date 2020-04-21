
# Is normality testing 'essentially useless'?
#https://stats.stackexchange.com/questions/2492/is-normality-testing-essentially-useless



#The last line checks which fraction of the simulations for every 
#sample size deviate significantly from normality. 
#So in 83% of the cases, a sample of 5000 observations deviates 
#significantly from normality according to Shapiro-Wilks. 
#Yet, if you see the qq plots, you would never ever decide on a deviation from normality. 
#Below you see as an example the qq-plots for one set of random samples 

set.seed(981677672)

x <- replicate(100, { # generates 100 different tests on each distribution
  c(shapiro.test(rnorm(10)+c(1,0,2,0,1))$p.value,   #$
    shapiro.test(rnorm(100)+c(1,0,2,0,1))$p.value,  #$
    shapiro.test(rnorm(1000)+c(1,0,2,0,1))$p.value, #$
    shapiro.test(rnorm(5000)+c(1,0,2,0,1))$p.value) #$
} # rnorm gives a random draw from the normal distribution
)
rownames(x) <- c("n10","n100","n1000","n5000")
rowMeans(x<0.05) # the proportion of significant deviations

#--------------------------------------------------------------------------------------------

qqnorm(x[1,]); qqline(y, col = 2)
qqnorm(x[2,]); qqline(y, col = 2)
qqnorm(x[3,]); qqline(y, col = 2)
qqnorm(x[4,]); qqline(y, col = 2)
#--------------------------------------------------------------------------------------------
library(nortest)
y <- replicate(100, { # generates 100 different tests on each distribution
  c(ad.test(rnorm(10)+c(1,0,2,0,1))$p.value,   #$
    ad.test(rnorm(100)+c(1,0,2,0,1))$p.value,  #$
    ad.test(rnorm(1000)+c(1,0,2,0,1))$p.value, #$
    ad.test(rnorm(5000)+c(1,0,2,0,1))$p.value) #$
} # rnorm gives a random draw from the normal distribution
)
rownames(y) <- c("n10","n100","n1000","n5000")
rowMeans(y<0.05) # the proportion of significant deviations
#--------------------------------------------------------------------------------------------
qqnorm(y[1,]); qqline(y, col = 2)
qqnorm(y[2,]); qqline(y, col = 2)
qqnorm(y[3,]); qqline(y, col = 2)
qqnorm(y[4,]); qqline(y, col = 2)
#--------------------------------------------------------------------------------------------


# Use the Shapiro Wilk because it's often powerful, widely available and many people are familiar with it (removing the need to explain in detail what it is if you use it in a paper) -- just don't use it under the illusion that it's "the best normality test". There isn't one best normality test.
