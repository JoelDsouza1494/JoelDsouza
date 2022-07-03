customer_loan=read.csv("C:\\Users\\HP\\OneDrive\\Desktop\\Data Science\\study material\\PROJECT\\customer Loan Prediction\\cutomer_loan.csv",
                       stringsAsFactors =F )
View(customer_loan)

#a.
str(customer_loan)

sum(is.na(customer_loan))
library(dplyr)

#b.
mutate(customer_loan,dti=debts/income)->cust_loan
View(cust_loan)
str(cust_loan)
cust_loan$dti=as.numeric(cust_loan$dti)
#c.
cust_loan=mutate(cust_loan,loan_decision_status=ifelse(loan_decision_type=="Denied",
                                                       0,
                                                       1))
cust_loan$loan_decision_status=as.factor(cust_loan$loan_decision_status)
str(cust_loan)

#d.

customer_loan_refined=select(cust_loan,3,4,6,7,8,11,13,14)
View(customer_loan_refined)
str(customer_loan_refined)
#e.
#library(mltools)
#library(data.table)
#Encoding=one_hot(as.data.table(customer_loan_refined[,-c(8)]))
#str(Encoding)
#customer_loan_refined=cbind(Encoding,loan_decision_staus=customer_loan_refined$loan_decision_status)
#View(customer_loan_refined)
#str(customer_loan_refined)

customer_loan_refined$gender=as.factor(customer_loan_refined$gender)
customer_loan_refined$marital_status=as.factor(customer_loan_refined$marital_status)
customer_loan_refined$occupation=as.factor(customer_loan_refined$occupation)
customer_loan_refined$loan_type=as.factor(customer_loan_refined$loan_type)

table(customer_loan_refined$gender)
customer_loan_refined$gender=as.numeric(customer_loan_refined$gender)
class(customer_loan_refined$gender)

customer_loan_refined$marital_status=as.numeric(customer_loan_refined$marital_status)
class(customer_loan_refined$marital_status)

customer_loan_refined$occupation=as.numeric(customer_loan_refined$occupation)
class(customer_loan_refined$occupation)

customer_loan_refined$loan_type=as.numeric(customer_loan_refined$loan_type)
class(customer_loan_refined$loan_type)

#Model Building
set.seed(1)
library(caTools)
split_tag=sample.split(customer_loan_refined$loan_decision_staus,SplitRatio = 0.7)
train=subset(customer_loan_refined,split_tag==T)
test=subset(customer_loan_refined,split_tag==F)
nrow(train)
nrow(test)
str(train)

#feature scaling
#train1=train %>% select(-loan_decision_status) %>% scale(center=TRUE,scale=TRUE)
#train1<- train1 %>% mutate(loan_decision_status=train$loan_decision_status)
train1<- train %>% select(-loan_decision_status) %>% mutate_all(scale)
train1<- train1 %>% mutate(loan_decision_status=train$loan_decision_status)
View(train1)

test1=test %>% select(-loan_decision_status) %>% mutate_all(scale)
test1=test1%>%mutate(loan_decision_status=test$loan_decision_status)
View(test1)

#PCA
library(caret)

pca=preProcess(x=train1[-8],method="pca",pcaComp = 2)
summary(pca)

train1_pca <- predict(pca,train1)
View(train1_pca)
train1_pca<- train1_pca[c(2,3,1)]
train1_pca=cbind(train1_pca,loan_decision_status=train1$loan_decision_status)
View(train1_pca)
test1_pca <- predict(pca,test1)
test1_pca<- test1_pca[c(2,3,1)]
test1_pca=cbind(test1_pca,loan_decision_status=test$loan_decision_status)
View(test1_pca)


#naive bayes
library(e1071)

model=naiveBayes(x=train1_pca[-4],y=train1_pca$loan_decision_status)
model


n_pred <- predict(model, newdata = test1_pca)
View(n_pred)

as.data.frame(n_pred)
#f. Build a confusion matrix for actual values and predicted values

cm<- table(test1_pca[,3], n_pred)
cm

confusionMatrix(cm)

