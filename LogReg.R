setwd("D:/MS/DataMining/HW2")
audit <- read.csv("audit.csv")
View(audit)
summary(audit)

levels(audit$Occupation)
audit.clean = audit#(na.omit(audit))

audit.clean$Occupation<- as.character(audit.clean$Occupation)
audit.clean$Occupation[is.na(audit.clean$Occupation)] <- "Unknown"
audit.clean$Occupation<- factor (audit.clean$Occupation)

audit.clean$Employment<- as.character(audit.clean$Employment)
audit.clean$Employment[is.na(audit.clean$Employment)] <- "Unknown"
audit.clean$Employment<- factor (audit.clean$Employment)

attach(audit.clean)
#1
summary(audit.clean)
dim(audit.clean)

#2
#Standard Deviation
standardDevs = c(Age = sd(audit.clean$Age), Income = sd(audit.clean$Income), 
                 Deductions = sd(audit.clean$Deductions), Hours = sd(audit.clean$Hours), RISK_Adjustment=sd(audit.clean$RISK_Adjustment))
standardDevs

#Summary
totSummary = c(Age = summary(audit.clean$Age), Income = summary(audit.clean$Income),
               Deductions = summary(audit.clean$Deductions), Hours = summary(audit.clean$Hours), RISK_Adjustment=summary(audit.clean$RISK_Adjustment))
totSummary

library("ggplot2")

ggplot(audit.clean, aes(x = RISK_Adjustment)) + geom_density()

ggplot(audit.clean, aes(x = Age)) + geom_density()

ggplot(audit.clean, aes(x = Income)) + geom_density()

ggplot(audit.clean, aes(x = Deductions)) + geom_density()

ggplot(audit.clean, aes(x = Hours)) + geom_density()

cor(audit.clean$Age, audit.clean$RISK_Adjustment)
ggplot(audit.clean, aes(x = Age, y = RISK_Adjustment)) + 
  geom_point(shape=1) +  geom_smooth(method=lm, se=FALSE)

cor(audit.clean$Income, audit.clean$RISK_Adjustment)
ggplot(audit.clean, aes(x = Income, y = RISK_Adjustment)) + 
  geom_point(shape=1) +  geom_smooth(method=lm, se=FALSE)

cor(audit.clean$Deductions, audit.clean$RISK_Adjustment)
ggplot(audit.clean, aes(x = Deductions, y = RISK_Adjustment)) + 
  geom_point(shape=1) +  geom_smooth(method=lm, se=FALSE)

cor(audit.clean$Hours, audit.clean$RISK_Adjustment)
ggplot(audit.clean, aes(x = Hours, y = RISK_Adjustment)) + 
  geom_point(shape=1) +  geom_smooth(method=lm, se=FALSE)


hist_Employment <- ggplot(audit.clean, aes(x=RISK_Adjustment, fill=Employment) )+ 
  geom_histogram(bins=30)

hist_Employment+xlab("Risk Adjustment")+ylab("Count")+ggtitle("Risk Adjustment for Employment")


hist_Gender <- ggplot(audit.clean, aes(x=RISK_Adjustment, fill=Gender) )+ 
  geom_histogram(bins=30)

hist_Gender+xlab("Risk Adjustment")+ylab("Count")+ggtitle("Risk Adjustment for Gender")


hist_Occupation<- ggplot(audit.clean, aes(x=RISK_Adjustment, fill=Occupation) )+ 
  geom_histogram(bins=30)

hist_Occupation+xlab("Risk Adjustment")+ylab("Count")+ggtitle("Risk Adjustment for Occupation")


hist_Marital<- ggplot(audit.clean, aes(x=RISK_Adjustment, fill=Marital) )+ 
  geom_histogram(bins=30)

hist_Marital+xlab("Risk Adjustment")+ylab("Count")+ggtitle("Risk Adjustment for Marital")


hist_Education<- ggplot(audit.clean, aes(x=RISK_Adjustment, fill=Education) )+ 
  geom_histogram(bins=30)

hist_Education+xlab("Risk Adjustment")+ylab("Count")+ggtitle("Risk Adjustment for Education")


#3
basemodel =  glm(TARGET_Adjusted ~ .,data = audit.clean,family="binomial")
summary(basemodel)

#with model matrix and no cross validation
Xdel = model.matrix(TARGET_Adjusted~.,data=audit.clean)[,-1] 

n.total=length(audit.clean$TARGET_Adjusted)
n.total

n.train=floor(n.total*(0.6))
n.train

n.test=n.total-n.train
n.test

train=sample(1:n.total,n.train)
xtrain = Xdel[train,]
xtest = Xdel[-train,]

ytrain = audit.clean$TARGET_Adjusted[train]
ytest = audit.clean$TARGET_Adjusted[-train]

m1 = glm(TARGET_Adjusted~.,family=binomial,data=data.frame(TARGET_Adjusted=ytrain,xtrain))
summary(m1)

ptest = predict(m1,newdata=data.frame(xtest),type="response")

data.frame(ytest,ptest)[1:10,]

btest=floor(ptest+0.5)

conf.matrix = table(ytest,btest)
conf.matrix

error=(conf.matrix[1,2]+conf.matrix[2,1])/n.test
error

#Using 10-fold cross validation
#Removing ID, RISK_Adjustment and
audit.del=audit.clean[,c(-1, -11)]
#audit.del[1:3,]
audit.train<-audit.del
#audit.train[1:3,]
Xdel = model.matrix(TARGET_Adjusted~Age+Employment+Education+Marital+Occupation+Income+Hours,data=audit.train)[,-1] 
#Xdel[1:3,]
#Age+Employment+Education+Marital+Occupation+Income+Gender+Deductions+Hours
n.total=length(audit.train$TARGET_Adjusted)
#n.total

n.train=floor(n.total*(0.8))
#n.train

n.test=n.total-n.train
#n.test

#train=sample(1:n.total,n.train)

xtrain = Xdel[1:n.train,]
xtest = Xdel[(n.train+1):n.total,]

ytrain = audit.train$TARGET_Adjusted[1:n.train]
ytest = audit.train$TARGET_Adjusted[(n.train+1):n.total]


#Create 10 equally size folds
folds <- cut(seq(1,nrow(xtrain)),breaks=10,labels=FALSE)

error = dim(10)
sens = dim(10)
spec = dim(10)
#Perform 10 fold cross validation

#xtrain<-xtrain[sample(nrow(xtrain)),]
df = data.frame(TARGET_Adjusted=ytrain,xtrain)
df = df[sample(nrow(df)),]
for(i in 1:10){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)

  #xtrain.cv = xtrain[testIndexes,]
  #xtest.cv = Xdel[-testIndexes,]
  
  #ytrain.cv = ytrain[testIndexes]
  #ytest.cv = audit.train$TARGET_Adjusted[-testIndexes]
  #Use the test and train data partitions however you desire...
  
  df.cv = df[testIndexes,]
  
  model.train = glm(formula=TARGET_Adjusted~.,family=binomial,data=df.cv)
  #summary(m1)
  #ptest = predict(m1,newdata=data.frame(xtest.cv),type="response")
  #btest=floor(ptest+0.5)
  #conf.matrix = table(ytest,btest)
  #error[i]=(conf.matrix[1,2]+conf.matrix[2,1])/length(ytest)
  
  #cut = 0.5
  #truepos <- ytest==1 & ptest>=cut 
  #trueneg <- ytest==0 & ptest<cut
  #sens[i] = sum(truepos)/sum(ytest==1)
  #spec[i] = sum(trueneg)/sum(ytest==0)
}

ytests = dim(2000)
index = 1
for(i in 1:10){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  

  ytest.cv = audit.train$TARGET_Adjusted[-testIndexes]
  #Use the test and train data partitions however you desire...
  
  #df.cv = df[testIndexes,]
  for(i in 1:200){
  ytests[index]= ytest.cv[i]
  index = index+1
  }
  
}

View(ytests)

ptest = predict(model.train,newdata=data.frame(xtest),type="response")
btest=floor(ptest+0.5)
conf.matrix = table(ytest,btest)
conf.matrix

#Accuracy
error=(conf.matrix[1,2]+conf.matrix[2,1])/n.test
error
acc = 1 - error
acc

#treating the given binary values "1/0" as "positive/negative" so switching TN with TP and FN with FP in conf.matrix
TN = conf.matrix[1,1]
TP = conf.matrix[2,2]
FN = conf.matrix[2,1]
FP = conf.matrix[1,2]

Precision = TP/(TP+FP)
Precision

Recall = TP/(TP+FN)
Recall

F1 = 2*(Precision*Recall)/(Precision+Recall)
F1

#testing function
ten.fold.cv <- function (data, query) {
audit.clean <- data
audit.del=audit.clean[,c(-1, -11)]
#audit.del[1:3,]
audit.train<-audit.del
#audit.train[1:3,]
Xdel = model.matrix(query,data=audit.train)[,-1] 
#Xdel[1:3,]
#Age+Employment+Education+Marital+Occupation+Income+Gender+Deductions+Hours
n.total=length(audit.train$TARGET_Adjusted)
#n.total

n.train=floor(n.total*(0.8))
#n.train

n.test=n.total-n.train
#n.test

#train=sample(1:n.total,n.train)

xtrain = Xdel[1:n.train,]
xtest = Xdel[(n.train+1):n.total,]

ytrain = audit.train$TARGET_Adjusted[1:n.train]
ytest = audit.train$TARGET_Adjusted[(n.train+1):n.total]


#Create 10 equally size folds
folds <- cut(seq(1,nrow(xtrain)),breaks=10,labels=FALSE)

error = dim(10)
sens = dim(10)
spec = dim(10)
#Perform 10 fold cross validation

#xtrain<-xtrain[sample(nrow(xtrain)),]
df = data.frame(TARGET_Adjusted=ytrain,xtrain)
df = df[sample(nrow(df)),]
for(i in 1:10){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  
  #xtrain.cv = xtrain[testIndexes,]
  #xtest.cv = Xdel[-testIndexes,]
  
  #ytrain.cv = ytrain[testIndexes]
  #ytest.cv = audit.train$TARGET_Adjusted[-testIndexes]
  #Use the test and train data partitions however you desire...
  
  df.cv = df[testIndexes,]
  
  model.train = glm(formula=TARGET_Adjusted~.,family=binomial,data=df.cv)
  #summary(m1)
  #ptest = predict(m1,newdata=data.frame(xtest.cv),type="response")
  #btest=floor(ptest+0.5)
  #conf.matrix = table(ytest,btest)
  #error[i]=(conf.matrix[1,2]+conf.matrix[2,1])/length(ytest)
  
  #cut = 0.5
  #truepos <- ytest==1 & ptest>=cut 
  #trueneg <- ytest==0 & ptest<cut
  #sens[i] = sum(truepos)/sum(ytest==1)
  #spec[i] = sum(trueneg)/sum(ytest==0)
}

ptest = predict(model.train,newdata=data.frame(xtest),type="response")
btest=floor(ptest+0.5)
conf.matrix = table(ytest,btest)
conf.matrix
return (conf.matrix)
}

formulaA = TARGET_Adjusted~Age+Employment+Education+Marital+Occupation+Income+Hours
conf.matrix <- ten.fold.cv(audit.clean, formulaA )

#Accuracy
error=(conf.matrix[1,2]+conf.matrix[2,1])/n.test
error
acc = 1 - error
acc

#treating the given binary values "1/0" as "positive/negative" so switching TN with TP and FN with FP in conf.matrix
TN = conf.matrix[1,1]
TP = conf.matrix[2,2]
FN = conf.matrix[2,1]
FP = conf.matrix[1,2]

Precision = TP/(TP+FP)
Precision

Recall = TP/(TP+FN)
Recall

F1 = 2*(Precision*Recall)/(Precision+Recall)
F1
#-----------

#TP and TN..to confirm
cut = 0.5
truepos <- ytest==1 & ptest>=cut 
trueneg <- ytest==0 & ptest<cut

#Sensitivty
sens = sum(truepos)/sum(ytest==1)
sens

#Sensitivity
spec = sum(trueneg)/sum(ytest==0)
spec

#Plotting LIFT
df=cbind(ptest,ytest)
df[1:20,]

rank.df=as.data.frame(df[order(ptest,decreasing=TRUE),])
colnames(rank.df) = c('predicted','actual')
rank.df[1:20,]

baserate=mean(ytest)
baserate

ax=dim(n.test)
ay.base=dim(n.test)
ay.pred=dim(n.test)
ax[1]=1
ay.base[1]=baserate
ay.pred[1]=rank.df$actual[1]
for (i in 2:n.test) {
  ax[i]=i
  ay.base[i]=baserate*i ## uniformly increase with rate xbar
  ay.pred[i]=ay.pred[i-1]+rank.df$actual[i]
}

df=cbind(rank.df,ay.pred,ay.base)
df[1:20,]

plot(ax,ay.pred,xlab="number of cases",ylab="number of successes",main="Lift: Cum successes sorted by pred val/success prob")
points(ax,ay.base,type="l")

#Plotting ROC
suppressWarnings(library(ROCR))
data=data.frame(predictions=ptest,labels=ytest)
data[1:10,]

pred <- prediction(data$predictions,data$labels)
str(pred)

perf <- performance(pred, "sens", "fpr")
str(perf)

plot(perf)

#Finding AUC
library(pROC)
auc <-  auc(data$labels, data$predictions)
auc

#4

library(MASS)

#Removing ID and Target_adjusted
audit.lm=audit.clean
audit.lm[1:3,]


audit.lm = model.matrix(RISK_Adjustment~poly(Age, degree=4) +Education + Marital + poly(Deductions, degree=5)+ Hours+TARGET_Adjusted,data=audit.lm)[,-1]
audit.lm[1:3,]

audit.df = data.frame(RISK_Adjustment=audit.clean$RISK_Adjustment,audit.lm)


#Full model
## leave-one-out cross validation
n = length(audit.clean$RISK_Adjustment)
n
error = dim(n)
for (k in 1:n) {
  train1 = c(1:n)
  train2 = train1[train1!=k] ## pick elements that are different from k
  m2 = lm(RISK_Adjustment ~., data=audit.df[train2,])
  pred = predict(m2, newdat=audit.df[-train2 ,])
  obs = audit.clean$RISK_Adjustment[-train2]
  error[k] = obs-pred
}

me=mean(error)
me 
rmse=sqrt(mean(error^2))
rmse

fitFull = lm(RISK_Adjustment ~ Age + Employment  + Education + Marital + Occupation+ Income+ Gender+ Deductions+ Hours, data=audit.clean)
stepAIC(fitFull, direction="backward")

#Trying to fit to model mentioned by STEPAIC

n = length(audit.clean$RISK_Adjustment)
n
error = dim(n)
for (k in 1:n) {
  train1 = c(1:n)
  train2 = train1[train1!=k] ## pick elements that are different from k
  m2 = lm(RISK_Adjustment ~poly(Age, degree=4) + Education + Marital + poly(Deductions, degree=5)+ Hours, data=audit.clean[train2,])
  pred = predict(m2, newdat=audit.clean[-train2 ,])
  obs = audit.clean$RISK_Adjustment[-train2]
  error[k] = obs-pred
}
me=mean(error)
me 
rmse=sqrt(mean(error^2))
rmse