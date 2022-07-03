census=read.csv("C:\\Users\\HP\\OneDrive\\Desktop\\Data Science\\study material\\PROJECT\\census\\census-income_.csv")
View(census)
str(census)
sum(is.na(census))

#Data pre-processing

census$workclass[28]
census[census== " ?"]<-NA

#Remove NA
sum(is.na(census))

census=na.omit(census)

#Removing white spaces
library(dplyr)
library(stringr)

census %>% mutate_if(is.character,str_trim)->census

View(census)

#convert character column to Factor

census$workclass=as.factor(census$workclass)
census$occupation=as.factor(census$occupation)
census$native.country=as.factor(census$native.country)
census$marital.status=as.factor(census$marital.status)
census$education=as.factor(census$education)
census$relationship=as.factor(census$relationship)
census$race=as.factor(census$race)
census$sex=as.factor(census$sex)
census$X=as.factor(census$X)

str(census)

#data manipulation
census_ed=census %>% select(education)
census_seq=census %>% select(age:relationship)
census_col=census %>%select(c(5,8,11))
male_gov=census %>% select(sex,workclass) %>% filter(sex=="Male" & workclass=="State-gov")
census_us=census %>% select(age,education,native.country) %>% filter(age==39 & (education=="Bachelors" | native.country=="United-States"))
View(census_us)
census_200=census %>% sample_n(200)
View(census_200)
census %>% count(workclass)
census %>% group_by(workclass) %>% summarise(mean(capital.gain))

#data visualization
library(ggplot2)

ggplot(data=census,aes(x=relationship,fill=race))+geom_bar()+
  labs(x="Categories of Relationships",y="Count of Categories")

ggplot(data=census,aes(x=relationship,fill=sex))+geom_bar(position = "dodge")+
  labs(title="Distribution of Relationships of Sex")

ggplot(data=census,aes(x=age,fill=X))+geom_histogram(bins=50)+
  labs(title = "Distribution of Age",fill="Yearly Income")+
  theme_bw()

ggplot(data = census,aes(x=capital.gain,y=hours.per.week,col=X))+geom_point(aplha=0.6,size=2)+
  labs(x="Capital Gain",y="Hours per week",title="Capital Gain Vs Hours per gain by Income",col="Yearly Income")

ggplot(data=census,aes(x=education,y=age,fill=sex))+geom_boxplot()+
  labs(title="Boxplot of Age by Education & Sex")

#Linear Regression
library(caTools)
set.seed(111)
split_tag=sample.split(census$hours.per.week,SplitRatio =0.7)
train=subset(census,split_tag==T)
test=subset(census,split_tag==F)

l_model=lm(hours.per.week~education.num,data=train)
summary(model)

pred_val=predict(l_model,newdata=test)
head(pred_val)

final_data=cbind(Actual=test$hours.per.week,Predicted=pred_val)

final_data=as.data.frame(final_data)
final_data$Actual-final_data$Predicted->error

final_data=cbind(final_data,error)

rmse=sqrt(mean(final_data$error)^2)
plot(census$education.num,census$hours.per.week)
abline(l_model)

#Logistic Regression
set.seed(123)
sample.split(census$X,SplitRatio = 0.65)->sample_tag
train=subset(census,sample_tag==T)
test=subset(census,sample_tag==F)

log_model=glm(X~occupation,data=train,family = "binomial")
summary(log_model)
pred_val=predict(log_model,newdata=test,type="response")
head(pred_val)
range(pred_val)

library(ROCR)
prediction(pred_val,test$X)->predict_log_roc
performance(predict_log_roc,"acc")->acc
plot(acc)

range(pred_val)
table(census$X)

lm.pred=ifelse(pred_val>0.47,">50k","<=50k")
table(lm.pred,test$X)
sum(diag(tab)/sum(tab))

library("caret")

#confusionMatrix(factor(lm.pred),test$X)

performance(predict_log_roc,"tpr","fpr")->roc
plot(roc)
performance(predict_log_roc,"auc")->auc
auc

auc=auc@y.values[[1]]
auc

library(caTools)

set.seed(222)
sample.split(census$X,SplitRatio = 0.8)->split_tag
train=subset(census,split_tag==T)
test=subset(census,split_tag==F)


log_mod=glm(X ~ age+workclass+education,data = train,family = "binomial")
summary(log_mod)

pred_val=predict(log_mod,newdata=test,method="response")
head(pred_val)
range(pred_val)

library(ROCR)
prediction(pred_val,test$X)->pred_log_roc
performance(predict_log_roc,"acc")->acc
plot(acc)

range(pred_val)
table(census$X)

lm.pred=ifelse(pred_val>0.45,">50k","<=50k")
table(lm.pred,test$X)->tab
sum(diag(tab))/sum(tab)
confusionMatrix(factor(lm.pred),test$X)

performance(predict_log_roc,"tpr","fpr")->auc
plot(auc)

auc_ROCR=performance(predict_log_roc,"auc")
auc_ROCR=auc@y.values[[1]]
auc_ROCR

predict(log_mod,newdata=data.frame(age=52,workclass="State-gov",education="HS-grad"),type="response")

#Decision Tree
library(caTools)
set.seed(333)
sample.split(census$X,SplitRatio = 0.7)->split_tag
train=subset(census,split_tag==T)
test=subset(census,split_tag==F)

library(rpart)
library(rpart.plot)

census_model=rpart(formula = X~.,
                   data=train,
                   method="class")
rpart.plot(x=census_model,type = 5,extra=0,tweak = 2)

class_prediction=predict(object=census_model,newdata = test,
                         type = "class")
tab=table(Prediction=class_prediction,Actual=test$X)
sum(diag(tab))/sum(tab)
confusionMatrix(class_prediction,test$X)

#RandomForest
set.seed(2)
sample.split(census$X,SplitRatio = 0.8)->split_tag
train=subset(census,split_tag==T)
test=subset(census,split_tag==F)
library(randomForest)
set.seed(3)
census_model=randomForest(formula=X~.,
                          data=train,
                          ntree=300)

print(census_model)

plot(census_model)
class_prediction=predict(object=census_model,
                         newdata=test,
                         type="class")
tab=table(Prediction=class_prediction,Actual=test$X)
sum(diag(tab))/sum(tab)

confusionMatrix(class_prediction,test$X)
