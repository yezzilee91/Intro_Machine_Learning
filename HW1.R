install.packages("tidyverse")

library(readr)
library(dplyr)
library(kernlab)
library(kknn)


credit <- read_delim(file.choose(), delim = "\t")
credit_svm <- ksvm(formula(R1 ~ .), data = credit)


data = data.frame(credit)

#finding number of rows and columns in data
nrow(data)
ncol(data)


# call ksvm. Vanilladot is a simple linear kernel.
model <- ksvm(
  as.matrix(data[,1:10]), as.factor(data[,11]),type="C-svc",kernel="vanilladot",
  C=0.05,scaled=TRUE
)

# calculate a1.am

a <- colSums(model@xmatrix[[1]] * model@coef[[1]])
round(a,4)

# calculate a0

a0 <- -model@b
round(a0,4)


# see what the model predicts
pred <- predict(model,data[,1:10])
pred

# see what fraction of the model's predictions match the actual classification

sum(pred == data[,11]) / nrow(data)


#2.2.3


for (k in c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15))  {
  predicted = rep(0,nrow(data))

  for (i in 1:nrow(data)){
    model = kknn(R1~., data[-i,], data[i,],  k=k, scale=TRUE)
    predicted[i] = as.integer(fitted(model)+0.5)
  }
  acc = sum(predicted == data$R1)/nrow(data)
  print(paste(as.character(k), round(acc,5)))
        }


#3.1

indx= sample(1:nrow(data), round(0.7*nrow(data)), replace=FALSE)

trainx= data[indx,1:11]
test= data[-indx,1:11]

val_indx= sample(1:nrow(test),round(0.5*nrow(test)), replace =FALSE)


valid=sample(1:nrow(test), round(0.5*nrow(test)), replace=FALSE)

validx=test[-valid, 1:11]
testx=test[valid, 1:11]


#cross validation
data$R1 = as.factor(data$R1)

for (i in c(1,2,3,4,5)) {
  model_cr=cv.kknn(R1 ~., data, k=i, kcv=5)
  print(paste('K=', i, 'Accuracy =', model_cr[[2]][1]))
  
}






#setting seed to produce reproducible results
set.seed(1)
#number of rows in data
m <- nrow(data)
#Randomly selecting  0.7 data as training and remaining for testing

val <- sample(1:m, size =round((0.7)*m), replace = FALSE)

d.train <-data[val,]#Training data
d.temp <-data[-val,] # remainging 30% of data

val2 <- sample(1:nrow(d.temp),size = round((0.5*nrow(d.temp))), replace = FALSE)

               
d.test = d.temp[val2,] #Test data : 15% of data
d.valid = d.temp[-val2,]#Valid data: 15% of data




#Train the linear kernel SVM
svm= ksvm(R1~., data=d.train, typ='C-svc', kernel='vanilladot', C=1, scaled=TRUE)
#get predictions on the valid data          
svm_pred <- predict(svm,d.valid[,1:10])
svm_pred

acc <- sum(svm_pred == d.valid$R1) /nrow(d.valid)
acc          

#Test data

svm_test = predict(svm,d.test[,1:10])
test_acc = sum(svm_test == d.test$R1) / nrow(d.test)
test_acc




# call ksvm. besseldot is a simple linear kernel.
modelbessel <- ksvm(
  as.matrix(data[,1:10]), as.factor(data[,11]),type="C-svc",kernel="besseldot",
  C=100,scaled=TRUE)
pred_bess <- predict(modelbessel,data[,1:10])
sum(pred_bess == data[,11]) / nrow(data)

