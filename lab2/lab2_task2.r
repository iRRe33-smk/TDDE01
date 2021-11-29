## Lab 2 - Task 2 ##

# TASK 1 and SETUP #
setwd("C:/Users/Robin/Desktop/TDDE01")
library(tree)
Dataframe<-read.csv("bank-full.csv",sep=";",stringsAsFactors=TRUE)
set.seed(12345)
df<-Dataframe
df$duration=c()# Remove 'duration'  
output <- Dataframe['y']
n <- dim(df)[1]

id <- sample(1:n, floor(0.4*n))
training_data <- df[id,]
df2 <- df[-id,]
n2 <- dim(df2)[1]

id2 <- sample(1:n2,floor(0.5*n2))
validation_data <- df2[id2,]
test_data <- df2[-id2,]

## TASK 2 ##

    # Create the tree model, plot, and print summary
fit<-tree(y~., data=training_data)
plot(fit)
text(fit, pretty=0)
summary(fit)

fit2 <- tree(y~.,data=training_data,control=tree.control(nrow(training_data),minsize=7000))
plot(fit2)
text(fit2, pretty=0)
summary(fit2)

fit3 <- tree(y~.,data=training_data,control=tree.control(nrow(training_data),mindev=0.0005))
plot(fit3)
text(fit3, pretty=0)
summary(fit3)

# Now use our 3 tree-models to predict Y on both training and validation data
# then calculate misclassification error

Yfit_tr1=predict(fit, newdata=training_data, type="class")
a1<-table(training_data$y,Yfit_tr1)
misc_rate_tr1 <- 1-sum(diag(a1))/sum(a1)

Yfit_tr2=predict(fit2, newdata=training_data, type="class")
a2<-table(training_data$y,Yfit_tr2)
misc_rate_tr2 <- 1-sum(diag(a2))/sum(a2)

Yfit_tr3=predict(fit3, newdata=training_data, type="class")
a3<-table(training_data$y,Yfit_tr3)
misc_rate_tr3 <- 1-sum(diag(a3))/sum(a3)

Yfit_va1=predict(fit, newdata=validation_data, type="class")
a1<-table(validation_data$y,Yfit_va1)
misc_rate_va1 <- 1-sum(diag(a1))/sum(a1)

Yfit_va2=predict(fit2, newdata=validation_data, type="class")
a2<-table(validation_data$y,Yfit_va2)
misc_rate_va2 <- 1-sum(diag(a2))/sum(a2)

Yfit_va3=predict(fit3, newdata=validation_data, type="class")
a3<-table(validation_data$y,Yfit_va3)
misc_rate_va3 <- 1-sum(diag(a3))/sum(a3)
 # Print error #
print(paste(("Misclassification_rate on training_data for model 1: "), misc_rate_tr1))
print(paste(("Misclassification_rate on validation_data for model 1: "), misc_rate_va1))
print(paste(("Misclassification_rate on training_data for model 2: "), misc_rate_tr2))
print(paste(("Misclassification_rate on validation_data for model 2: "), misc_rate_va2))
print(paste(("Misclassification_rate on training_data for model 3: "), misc_rate_tr3))
print(paste(("Misclassification_rate on validation_data for model 3: "), misc_rate_va3))

## TASK 3 ##
a <- tree(as.factor(y)~., data = training_data,control = tree.control(nrow(training_data),mindev=0.0005))
plot(a)
text(a, pretty=0)
summary(a)
trainScore=rep(0,50)
testScore=rep(0,50)
for(i in 2:50) {
  prunedTree=prune.tree(a,best=i)
  pred=predict(prunedTree, newdata=validation_data, type="tree")
  trainScore[i]=deviance(prunedTree)
  testScore[i]=deviance(pred)
  #plot(prunedTree)
  #text(a, pretty=0)
}
plot(2:50, trainScore[2:50], type="b", col="red", ylim=c(min(testScore[-1]),max(trainScore[-1])),xlab ="Tree depth k",ylab="Deviance",main="Deviance depending on depth of tree")
points(2:50, testScore[2:50], type="b", col="blue")
finalTree=prune.tree(a, best=21)  ## Found that 18 leaves gave us the best fit
Yfit=predict(finalTree, newdata=validation_data, type="class")
plot(finalTree)
text(a, pretty=1)
## TASK 4 ##

optTree_for_Test <-tree(y~., data=training_data, control=tree.control(nrow(training_data),mindev=0.0005))
PrunedOptTree_for_Test <- prune.tree(fit,best=21)
set.seed(12345)
fit <- predict(optTree_for_Test,newdata=test_data,type="class")
tab <- table(test_data$y,fit)
tab
accuracy <- sum(diag(tab))/sum(tab)
recall_rate <- tab[2,2]/(tab[2,2]+tab[2,1])
f1_score <- (2*accuracy*recall_rate)/(accuracy + recall_rate)

print(paste(("Accuracy for optimal model: "), accuracy))
print(paste(("Recall-rate for optimal model: "), recall_rate))
print(paste(("F1 Score for optimal model: "), f1_score))

## TASK 5 ##
Tree_task5 <-tree(y~., data=training_data, control=tree.control(nrow(training_data),mindev=0.0005))
set.seed(12345)
pred_fit <- predict(Tree_task5,newdata=test_data,type="vector")
# Loop over each row and replace
new_pred_fit <- vector(length = nrow(test_data))
for(i in 1:nrow(pred_fit)){
  if( (pred_fit[i,2]/pred_fit[i,1])>5){
    pred_fit[i,2] = 1
    pred_fit[i,1] = 0
    new_pred_fit[i] = 'Yes'
  }else{
    pred_fit[i,2] = 0
    pred_fit[i,1] = 1
    new_pred_fit[i] = 'No'
  }
}
tab2 <- table(test_data$y,new_pred_fit)
## Use probability from fit type: vector to get probs, then change class if yes>=5*No
accuracy2 <- sum(diag(tab2))/sum(tab2)
recall_rate2 <- tab2[2,2]/(tab2[2,2]+tab2[2,1])
f1_score2 <- (2*accuracy2*recall_rate2)/(accuracy2 + recall_rate2)

print(paste(("Accuracy for optimal model: "), accuracy2))
print(paste(("Recall-rate for optimal model: "), recall_rate2))
print(paste(("F1 Score for optimal model: "), f1_score2))


## TASK 6 ##
optTree <-tree(y~., data=training_data, control=tree.control(nrow(training_data),mindev=0.0005)) 
optTree=prune.tree(optTree, best=21)  ## Found that 21 leaves gave us the best fit
test_predict <- predict(optTree,newdata=test_data,type="vector")
pi_v <- seq(0.05,0.95,0.05)  
tpr <- rep(0,length(pi_v))        # TRP = TP / (TP + FN)
fpr <- rep(0,length(pi_v))        # FPR = FP / (FP + TN)
for (i in 1:length(pi_v)){
  # If we assume "Good" = 0 and Bad = 1, then
  test_predict.tree <- ifelse(test_predict[,1]>pi_v[i],"no","yes")
  test_predict.matrix = table(test_predict.tree, test_data$y)
  #TRP = TP / TP+FN, FPR =  FP / FP + TN, as said the n
  if(nrow(test_predict.matrix) > 1){
    tpr[i] = test_predict.matrix[2,2] / (test_predict.matrix[2,2] + test_predict.matrix[2,1])
    fpr[i] = test_predict.matrix[1,2] / (test_predict.matrix[1,1] + test_predict.matrix[1,2])
  }
}
plot(c(0, fpr, 1), c(0, tpr, 1), xlim = c(0,1), ylim = c(0,1),sub="Each point represent an increment of 0.05 in the range [0.05,0.95]",main="Recieving Operator Characteristics (ROC) curve", type = "b",xlab="False Positive Ratio (FPR)",ylab="True Positive Ratio (TRP)")
tpr
fpr
