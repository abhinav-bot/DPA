install.packages("glmnet")
library(glmnet)
library(caret)
lass<- read.csv("C:/Users/mrame/OneDrive/Desktop/Labelling files/finalexamDemoDiet.csv", stringsAsFactors = F)
str(lass)
lass<-lass[,-c(1,2)]
set.seed(123)
trainLassoInd <- createDataPartition(finalLasso$is_diabetic, p=0.80, list=FALSE)
nrow(finalexamDemoDietNUM)
trainLasso = lass[trainLassoInd,]
testLasso = lass[-trainLassoInd,]
nrow(trainLasso)
nrow(testLasso)
colnames(trainLasso)
x <- model.matrix(is_diabetic~.,trainLasso)
cv.out <- cv.glmnet(x,trainLasso$is_diabetic,alpha=1,family='binomial',type.measure = 'mse' )
plot(cv.out)
lambda_min <- cv.out$lambda.min #minvalue
lambdal <- cv.out$lambdal#lambda value
coef(cv.out,s=lambdal)
x_test <- model.matrix(is_diabetic~.,testLasso)
lasso_prob <- predict(cv.out,newx = x_test,s=lambdal,type='response')
nrow(lasso_prob)
lasso.pred <- ifelse(lasso_prob > 0.5,1,0) #prediction
#confusion matrix
nrow(testLasso)
nrow(lasso.pred)
lassot<-table(pred=lasso.pred,true=testLasso$is_diabetic)
mean(lasso.pred==testLasso$is_diabetic)
confusionMatrix(lassot)
