#Example of robust linear regression
#Note that this example does not include checking assumptions of the model, but is intended only to compare the results of ordinary linear regression and robust regression when there is a data point with high influence.

Input=("
X    Y
0    0
1    2
2    5
3    6
4    9
5   10
6   11
7   14
8   16
9   20
10  44
")

Data = read.table(textConnection(Input),header=TRUE)

#Linear regression

model = lm(Y ~ X,
           data = Data)

summary(model)
R2     = summary(model)$r.squared
t2     = paste0("R-squared: ", signif(R2, digits=3))
t3     = paste0("Intercept: ", signif(coef(model)[1], digits=3))
t4     = paste0("Slope: ", signif(coef(model)[2], digits=3))
plot(Y ~ X,
     data = Data,
     pch  = 16)
abline(model,
       col="blue",
       lwd=2)
text(0, 33, labels = t2, pos=4)
text(0, 28, labels = t3, pos=4)
text(0, 23, labels = t4, pos=4)

barplot(cooks.distance(model))
plot(fitted(model),residuals(model))

#Robust regression
library(robustbase)
model.r = lmrob(Y ~ X,
                data = Data)
summary(model.r)
#Calculate statistics and plot
model.null = lmrob(Y ~ 1,
                   data = Data)

Pvalue = anova(model.r, model.null)[4][2,1]
R2     = summary(model.r)$r.squared
t1     = paste0("p-value: ", signif(Pvalue, digits=3))
t2     = paste0("R-squared: ", signif(R2, digits=3))
t3     = paste0("Intercept: ", signif(coefficients(model.r)[1], digits=3))
t4     = paste0("Slope: ", signif(coefficients(model.r)[2], digits=3))

plot(Y ~ X,
     data = Data,
     pch  = 16)
abline(model.r,
       col="blue",
       lwd=2)
text(0, 38, labels = t1, pos=4)
text(0, 33, labels = t2, pos=4)
text(0, 28, labels = t3, pos=4)
text(0, 23, labels = t4, pos=4)






Packages used in this chapter


The packages used in this chapter include:
  
  •  robustbase

•  psych

•  minpack.lm

•  car

•  rcompanion



The following commands will install these packages if they are not already installed:
  
  
  if(!require(robustbase)){install.packages("robustbase")}
if(!require(pysch)){install.packages("pysch")}
if(!require(nlstools)){install.packages("nlstools")}
if(!require(minpack.lm)){install.packages("minpack.lm")}
if(!require(car)){install.packages("car")}
if(!require(rcompanion)){install.packages("rcompanion")}


More complex experimental designs



Except for t-tests, the approach of this book for parametric statistics has been to develop linear models (with the lm function) or mixed effects models (with the nlme or lme4 packages) and then to apply analysis of variance, model checking, and post-hoc testing.



These relatively simple models can be expanded to include more independent variables of either continuous or factor type.  Likewise, more complex designs can include nested and crossed factors.  Some traditional experimental designs include Latin square, split plot, and incomplete block.


Analysis of co-variance.



Analysis of covariance combines continuous independent variables with factor independent variables. 



A related design is the paired watershed design which, while it was developed for watershed studies, can be adapted to various situations in a variety of fields.



“Analysis of Covariance” in Mangiafico, S.S. 2015. An R Companion for the Handbook of Biological Statistics, version 1.09. rcompanion.org/rcompanion/e_04.html.



Clausen, J.C. and J. Spooner. 1993. Paired Watershed Study Design. 841-F-93-009. United States Environmental Protection Agency, Office of Water. Washington, DC. nepis.epa.gov/Exe/ZyPURL.cgi?Dockey=20004PR6.TXT.



Dressing, S.A. and D.W. Meals. 2005. Designing water quality monitoring programs for watershed projects. U.S. Environmental Protection Agency. Fairfax, VA. www.epa.gov/sites/production/files/2015-10/documents/technote2_wq_monitoring.pdf.


Nonlinear regression and curvilinear regression



Nonlinear and curvilinear regression are similar to linear regression, except that the relationship between the dependent variable and the independent variable is curved in some way.  Specific methods include polynomial regression, spline regression, and nonlinear regression.



Polynomial regression was covered briefly in the previous chapter, while some examples of curvilinear regression are shown below in the “Linear plateau and quadratic plateau models” section in this chapter. 



For further discussion, see:
  
  
  
  “Curvilinear Regression” in Mangiafico, S.S. 2015. An R Companion for the Handbook of Biological Statistics, version 1.09. rcompanion.org/rcompanion/e_03.html.


Multiple regression



Multiple regression is similar to linear regression, except that the model contains multiple independent variables.



The polynomial regression example in the previous chapter is one example of multiple regression. 



For further discussion, see:
  
  
  
  “Multiple Regression” in Mangiafico, S.S. 2015. An R Companion for the Handbook of Biological Statistics, version 1.09. rcompanion.org/rcompanion/e_05.html.


Logistic regression



Logistic regression is similar to linear regression, except that the dependent variable is categorical.  In standard logistic regression, the dependent variable has only two levels, and in multinomial logistic regression, the dependent variable can have more than two levels.



For examples of logistic regression, see the chapter Models for Nominal Data in this book.


Analysis of count data



When the dependent variable is a counted quantity and not a measured quantity, it is appropriate to use Poisson regression or related techniques.



For a discussion of when standard parametric approaches may or may not be used with count data, see the “Count data may not be appropriate for common parametric tests” section in the Introduction to Parametric Tests chapter.



For examples of appropriate ways to model count data, see the Regression for Count Data chapter in this book.


Robust techniques



Standard parametric analyses can be sensitive to outliers and other deviations from assumptions of the analyses.  The example below shows that a single point can affect the predicted line from standard linear regression.  It is said that this point has high leverage, as so has high influence in the model.



One approach in these cases is to use nonparametric techniques. 



Another approach is to adjust the model to better fit the data.



A third way is to determine if certain data points are true outliers, and then give them further examination. 



A fourth way is to use a robust technique such as robust regression.  The references below discuss robust regression and robust techniques appropriate for designs lending themselves to an analysis of variance approach.



“One-way Analysis with Permutation Test” in Mangiafico, S.S. 2015. An R Companion for the Handbook of Biological Statistics, version 1.09. rcompanion.org/rcompanion/d_06a.html.



“Two-way Anova with Robust Estimation” in Mangiafico, S.S. 2015. An R Companion for the Handbook of Biological Statistics, version 1.09. rcompanion.org/rcompanion/d_08a.html.



Search for Robust regression in the following:
  
  “Correlation and Linear Regression” in Mangiafico, S.S. 2015. An R Companion for the Handbook of Biological Statistics, version 1.09. rcompanion.org/rcompanion/e_01.html.


Example of robust linear regression

Note that this example does not include checking assumptions of the model, but is intended only to compare the results of ordinary linear regression and robust regression when there is a data point with high influence.


Input=("
X    Y
0    0
1    2
2    5
3    6
4    9
5   10
6   11
7   14
8   16
9   20
10  44
")

Data = read.table(textConnection(Input),header=TRUE)

Linear regression


model = lm(Y ~ X,
           data = Data)

summary(model)


Coefficients:
  Estimate Std. Error t value Pr(>|t|)   
(Intercept)  -3.1364     3.6614  -0.857 0.413888   
X             3.1182     0.6189   5.038 0.000701 ***
  
  Residual standard error: 6.491 on 9 degrees of freedom
Multiple R-squared:  0.7383,  Adjusted R-squared:  0.7092
F-statistic: 25.39 on 1 and 9 DF,  p-value: 0.0007013


Calculate statistics and plot


Pvalue = pf(summary(model)$fstatistic[1],
            summary(model)$fstatistic[2],
            summary(model)$fstatistic[3],
            lower.tail = FALSE)

R2     = summary(model)$r.squared

t1     = paste0("p-value: ", signif(Pvalue, digits=3))
t2     = paste0("R-squared: ", signif(R2, digits=3))
t3     = paste0("Intercept: ", signif(coef(model)[1], digits=3))
t4     = paste0("Slope: ", signif(coef(model)[2], digits=3))

plot(Y ~ X,
     data = Data,
     pch  = 16)
abline(model,
       col="blue",
       lwd=2)
text(0, 38, labels = t1, pos=4)
text(0, 33, labels = t2, pos=4)
text(0, 28, labels = t3, pos=4)
text(0, 23, labels = t4, pos=4)





Cook’s distance

Cook’s distance is a measure of influence of each data point, specifically determining how much predicted values would change if the observation were deleted.  It is sometimes recommended that a Cook’s distance of 1 or more merits further examination of the data point.  However, there are other recommendations for critical values for Cook’s distance.



In this case, the high Cook’s distance for observation 11 makes it a candidate for further evaluation.


barplot(cooks.distance(model))





Plot of residuals

This plot suggests that the residuals are not independent of the fitted values.  We would probably want to adjust the model by adding additional terms, using a nonlinear or curvilinear approach, or using a robust analysis to minimize the influence of certain data points.


plot(fitted(model),
     residuals(model))





Robust regression


library(robustbase)

model.r = lmrob(Y ~ X,
                data = Data)

summary(model.r)


Coefficients:
  Estimate Std. Error t value Pr(>|t|)   
(Intercept)  0.09015    0.32804   0.275     0.79   
X            2.04249    0.10970  18.619  1.7e-08 ***
  
  Robust residual standard error: 0.9378
Multiple R-squared:  0.9817,  Adjusted R-squared:  0.9796


Calculate statistics and plot


model.null = lmrob(Y ~ 1,
                   data = Data)

Pvalue = anova(model.r, model.null)[4][2,1]

R2     = summary(model.r)$r.squared

t1     = paste0("p-value: ", signif(Pvalue, digits=3))
t2     = paste0("R-squared: ", signif(R2, digits=3))
t3     = paste0("Intercept: ", signif(coefficients(model.r)[1], digits=3))
t4     = paste0("Slope: ", signif(coefficients(model.r)[2], digits=3))

plot(Y ~ X,
     data = Data,
     pch  = 16)
abline(model.r,
       col="blue",
       lwd=2)
text(0, 38, labels = t1, pos=4)
text(0, 33, labels = t2, pos=4)
text(0, 28, labels = t3, pos=4)
text(0, 23, labels = t4, pos=4)

# Kendall Theil nonparametric linear regression
# Kendall–Theil regression is a completely nonparametric approach to linear regression.  It is robust to outliers in the y values.  It simply computes all the lines between each pair of points, and uses the median of the slopes of these lines.  This method is sometimes called Theil–Sen.  A modified, and preferred, method is named after Siegel.
# The mblm function in the mblm package uses the Siegel method by default.  The Theil–Sen procedure can be chosen with the repeated=FALSE option. See library(mblm); ?mblm for more details.

library(mblm)

model = mblm(Y ~ X,
             data=Data)

summary(model)

Pvalue    = as.numeric(summary(model)$coefficients[2,4])
Intercept = as.numeric(summary(model)$coefficients[1,1])
Slope     = as.numeric(summary(model)$coefficients[2,1])
R2        = NULL

t1     = paste0("p-value: ", signif(Pvalue, digits=3))
t2     = paste0("R-squared: ", "NULL")
t3     = paste0("Intercept: ", signif(Intercept, digits=3))
t4     = paste0("Slope: ", signif(Slope, digits=3))

plot(Y ~ X,
     data = Data,
     pch  = 16)

abline(model,
       col="blue",
       lwd=2)
text(1, 40, labels = t1, pos=4)
text(1, 36, labels = t2, pos=4)
text(1, 32, labels = t3, pos=4)
text(1, 28, labels = t4, pos=4)



