cmc<-read.csv("cmc.txt",header = TRUE)
attach(cmc)
#First, install all the packages needed.
install.packages("caret")
library(caret)
install.packages("randomForest")
library(randomForest)
library(rpart)
library("ggplot2")
install.packages("party")
library(party)
install.packages("randomForestSRC")
library(randomForestSRC)
install.packages("ada")
library(ada)
install.packages("arules")
set.seed(123)

length(cmc$result[which(cmc$result==1)])
length(cmc$result[which(cmc$result==2)])
length(cmc$result[which(cmc$result==3)])

#Before do the histograms, transform the data to the numerical type.
par(mfrow=c(2,5))
cmc$result<-as.numeric(cmc$result)
cmc$Weducation<-as.numeric(cmc$Weducation)
cmc$Heducation<-as.numeric(cmc$Heducation)
cmc$Wreligion<-as.numeric(cmc$Wreligion)
cmc$Wwork<-as.numeric(cmc$Wwork)
cmc$Hoccupation<-as.numeric(cmc$Hoccupation)
cmc$living<-as.numeric(cmc$living)
cmc$media<-as.numeric(cmc$media)
cmc$age<-as.numeric(cmc$age)

hist(cmc$age,xlab="age",main="age")
hist(as.numeric(cmc$Weducation),xlab="Weducation",main="Weducation")
hist(as.numeric(cmc$Heducation),xlab="Heducation",main="Heducation")
hist(as.numeric(cmc$Wreligion),xlab="Wreligion",main="Wreligion")
hist(as.numeric(cmc$Wwork),xlab="Wwork",main="Wwork")
hist(as.numeric(cmc$Hoccupation),xlab="Hoccupation",main="Hoccupation")
hist(as.numeric(cmc$living),xlab="living",main="living")
hist(as.numeric(cmc$media),xlab="media",main="media")
hist(cmc$children,xlab="children",main="children")
hist(as.numeric(cmc$result),xlab="result",main="result")

cmc$result<-as.factor(result)
cmc$Weducation<-as.factor(Weducation)
cmc$Heducation<-as.factor(Heducation)
cmc$Wreligion<-as.factor(Wreligion)
cmc$Wwork<-as.factor(Wwork)
cmc$Hoccupation<-as.factor(Hoccupation)
cmc$living<-as.factor(living)
cmc$media<-as.factor(media)

#dimension reduction
#Random forest
fit.a<-randomForest(cmc$result~., data=cmc,ntree=500,keep.forest=TRUE,importance=TRUE)
fit.a
varImp(fit.a)
varImpPlot(fit.a,type=2,main ="contraceptive method choice VI for overall")

par(mfrow=c(1,1))
plot(fit.a, main="error rate vs. # of trees")
legend("topright",c("total","none","short","long"),col=c("1","2","3","4"),pch=1,title="error")

#only use data by long term& short term, also transform the response variable as numeric type at first:
cmc$result<-as.numeric(cmc$result)
cmm<-cmc[cmc$result==2|cmc$result==3,]
cmm$result<-as.factor(cmm$result)

fit.b<-randomForest(cmm$result~., data=cmm,ntree=500,keep.forest=TRUE,importance=TRUE)
fit.b
varImp(fit.b,type=2)
varImpPlot(fit.b,type=2,main ="contraceptive method choice VI for 2&3")

plot(fit.b, main="error rate(2&3) vs. # of trees")
legend("topright",c("total","short","long"),col=c("1","2","3"),pch=1,title="error")

#define data by use& no use, also transform the response variable as numeric type at first:
cmb<-cmc
cmb$result[which(cmb$result=="2")]<-4
cmb$result[which(cmb$result=="3")]<-4
cmb$result<-as.factor(cmb$result)

fit.c<-randomForest(cmb$result~., data=cmb,ntree=500,keep.forest=TRUE,importance=TRUE)
fit.c
varImp(fit.c,type=2)
varImpPlot(fit.c,type=2,main ="contraceptive method choice VI for 1&(2+3)")

plot(fit.c, main="error rate(1&(2+3)) vs. # of trees")
legend("topright",c("total","no use","use"),col=c("1","2","3"),pch=1,title="error")

#do RandomForestSRC function, it needs to define a data frame at first:
cmc$result<-as.factor(cmc$result)
abc<-data.frame(x1=cmc$age,x2=cmc$Weducation,x3=cmc$Heducation,x4=cmc$children,x5=cmc$Wreligion,x6=cmc$Wwork,x7=cmc$Hoccupation,x8=cmc$living,x9=cmc$media,y=cmc$result)
rfsrc.a<-rfsrc(y~x1+x2+x3+x4+x5+x6+x7+x8+x9,data=abc,ntree=5000,OOB=TRUE,coerce.factor =abc$y)
var.select(y~.,type=2,data=abc,ntree=1000,outcome.target=abc$y)

#stratify sampling, for long term& short term& no use :
set1<-cmc[cmc$result=="1",]
set2<-cmc[cmc$result=="2",]
set3<-cmc[cmc$result=="3",]
dim(set1)  
dim(set2)
dim(set3)
629*3/4
333*3/4
511*3/4

set.seed(123)  
training1<-sample(1:629,472)
test1<-(1:629)[-training1]

training2<-sample(1:333,250)
test2<-(1:333)[-training2]

training3<-sample(1:511,383)
test3<-(1:511)[-training3]

train<-rbind(set1[training1,],set2[training2,],set3[training3,])
test<-rbind(set1[test1,], set2[test2,],set3[test3,])

#do classification tree and calculate the predict error rate
# tree growing and pruning with training data
my.control <- rpart.control(cp=0, xval=10)
fit1<- rpart(train$result ~ ., data=train, method="class",control=my.control)
printcp(fit1)  
0.75671+0.026024

tree5<-prune(fit1,cp=0.008)
plot(tree5,uniform=T, margin=0.2)
text(tree5,use.n=T)
pred5<-predict(tree5,newdata=test,type="class")
ta<-table(test$result,pred5)
error5<-ta[1,2]+ta[2,1]+ta[1,3]+ta[2,3]+ta[3,1]+ta[3,2]
error.rate.a<-error5/length(test$result)
error.rate.a

#then to compare with random forest prediction error rate:
rfa<-randomForest(train$result~., data=train)
rfa.pred<-predict(rfa,test)
tb<-table(test$result,rfa.pred)
errortb<-tb[1,2]+tb[2,1]+tb[1,3]+tb[2,3]+tb[3,1]+tb[3,2]
error.rate.b<-errortb/length(test$result)
error.rate.b

#stratify sampling, for use& no use :
set4<-cmb[cmb$result=="1",]
set5<-cmb[cmb$result=="4",]
dim(set4)  
dim(set5)
629*3/4
844*3/4

training4<-sample(1:629,472)
test4<-(1:629)[-training4]

training5<-sample(1:844,633)
test5<-(1:844)[-training5]

train1<-rbind(set4[training4,],set5[training5,])
test1<-rbind(set4[test4,], set5[test5,])

#and then, use two ways of ada boost, count the prediction error rate:
adam<-ada(train1$result~.,data=train1,iter=50,loss="e",type="real", control=my.control)
print(adam)
adan<-ada(train1$result~.,data=train1,iter=50,loss="e",type="discrete", control=my.control)
print(adan)

# test1[,-10] is the variables without response variable, 
# test1[,10] is just the one response variable
adann<-addtest(x=adan, test.x=test1[,-10], test.y=test1[,10])
summary(adann,n.iter=50)

plot(adann,kappa=T, test=T)
pred<-predict(adan,test1[,-10])
tf<-table(test1$result,pred)
#do the same thing above to compare the prediction error rate with classification tree and random forest.
errortf<-tf[1,2]+tf[2,1]
error.rate.f<-errortf/length(test1$result)
error.rate.f

varplot(adan,type = "none")

fit2<- rpart(train1$result ~ ., data=train1, method="class",control=my.control)
printcp(fit2)  
0.72881+0.032610

tree3<-prune(fit2,cp=0.022)
plot(tree3,uniform=T, margin=0.2)
text(tree3,use.n=T)
pred3<-predict(tree3,newdata=test1,type="class")
td<-table(test1$result,pred3)
error3<-td[1,2]+td[2,1]
error.rate.d<-error3/length(test1$result)
error.rate.d

rfb<-randomForest(train1$result~., data=train1)
rfb.pred<-predict(rfb,test1)
te<-table(test1$result,rfb.pred)
errorte<-te[1,2]+te[2,1]
error.rate.e<-errorte/length(test1$result)
error.rate.e

varImp(rfb,type=2)
varImpPlot(rfb,type=2,main ="train:contraceptive method choice VI for 1&(2+3)")

#then, stratify sampling, for long term& short term only:
set6<-cmm[cmm$result=="2",]
set7<-cmm[cmm$result=="3",]
dim(set6) 
dim(set7)
333*3/4
511*3/4
training6<-sample(1:333,245)
test6<-(1:333)[-training6]

training7<-sample(1:511,383)
test7<-(1:511)[-training7]

train2<-rbind(set6[training6,],set7[training7,])
test2<-rbind(set6[test6,], set7[test7,])

#then, repeat the following steps to calculate the prediction error rate and compare it with random forest and classification tree.
fit.3<-rpart(train2$result ~ ., data=train2, method="class",control=my.control)
printcp(fit.3)
0.91429+0.048997
tree7<-prune(fit.3,cp=0.025)
plot(tree7,uniform=T, margin=0.2)
text(tree7,use.n=T)
pred7<-predict(tree7,newdata=test2,type="class")
tw<-table(test2$result,pred7)
error9<-tw[1,2]+tw[2,1]
error.rate.w<-error9/length(test2$result)
error.rate.w

rfc<-randomForest(train2$result~., data=train2)
rfc.pred<-predict(rfc,test2)
ty<-table(test2$result,rfc.pred)
errorty<-ty[1,2]+ty[2,1]
error.rate.y<-errorty/length(test2$result)
error.rate.y
