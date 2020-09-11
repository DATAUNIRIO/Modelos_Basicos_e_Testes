#----------------------------------------------------
#https://online.stat.psu.edu/stat504/
### Berkeley admissions data for log-linear models
### Lessons 4 & 5
### See also berkeley.R in Lesson 4
#----------------------------------------------------

### Dataset already exist in R library
UCBAdmissions 

### To test the odds-ratios in the marginal table and each of the subtables
library(vcd)

### Two ways of fitting a log-linear model of complete independence

### Via loglin() function
berk.ind<-loglin(UCBAdmissions, list(1,2,3), fit=TRUE, param=TRUE)
berk.ind

### Via glm() function #######
berk.data<-as.data.frame(UCBAdmissions)
berk.data
berk.ind<-glm(berk.data$Freq~berk.data$Admit+berk.data$Gender+berk.data$Dept, family=poisson())
summary(berk.ind)
fits<-fitted(berk.ind)
resids <- residuals(berk.ind,type="pearson")
h <- lm.influence(berk.ind)$hat
adjresids <- resids/sqrt(1-h)
round(cbind(berk.data$Freq,fits,adjresids),2)

### Via loglin() function
berk.ind<-loglin(UCBAdmissions, list(1,2,3), fit=TRUE, param=TRUE)
berk.ind

##### Saturated log-linear model 
### via loglin()
berk.sat<-loglin(UCBAdmissions, list(c(1,2,3)), fit=TRUE, param=TRUE)
berk.sat

### via glm()
berk.sat<-glm(berk.data$Freq~berk.data$Admit*berk.data$Gender*berk.data$Dept, family=poisson())
summary(berk.sat)
fitted(berk.sat)