incomedata<-read.csv("adult.csv",header=TRUE)
attach(incomedata)
age[which(age=="?")]
workclass[which(workclass=="?")]<-NA
levels(education)
levels(marital.status)
levels(occupation)
occupation[which(occupation=="?")]<-NA
levels(relationship)
levels(race)
levels(sex)
class(education.num)
capital.gain[which(capital.gain=="?")]
capital.loss[which(capital.loss=="?")]
hours.per.week[which(hours.per.week=="?")]
levels(native.country)
native.country[which(native.country=="?")]<-NA
incomedata$income<-factor(incomedata$income,levels=c("<=50K",">50K"),labels=c("0","1"))))

install.packages("caret")
library(caret)
install.packages("randomForest")
library(randomForest)
library(rpart)


#dimension reduction
fit.r<-randomForest(~., data=incomedata,norm.votes=F)
varImp(fit.r)
varImpPlot(fit.r,type=2)

my.control<-rpart.control(cp=0,xval=10)
fit.p<-rpart(income~.,data=incomedata,method="class",control=my.control)
printcp(fit.p)

0.57709+0.0079606=0.5850506
tree9<-prune(fit.p,cp=(1.7855e-03 +1.6580e-03)/2)
plot(tree9,uniform = T,margin=0.2)
text(tree9,use.n = T)

pred9<-predict(tree9,incomedata,type="class")
table(incomedata$income,pred9)
error9<-table(incomedata$income,pred9)[1,2]+table(incomedata$income,pred9)[2,1]
error.rate<-error9/length(income)

fit.pp<-rpart(income~.,data=incomedata,parms =list(prior=c(0.25, 0.75)), method="class",control=my.control)
printcp(fit.pp)
0.50509+0.0080517

prior.tree9<-prune(fit.pp,cp=(2.2083e-03+2.5963e-03)/2)
plot(prior.tree9,uniform = T,margin=0.2)
text(prior.tree9,use.n = T)
prior.pred9<-predict(prior.tree9,incomedata,type="class")
table(incomedata$income,prior.pred9)
prior.error9<-table(incomedata$income,prior.pred9)[1,2]+table(incomedata$income,prior.pred9)[2,1]
prior.error.rate<-prior.error9/length(income)