setwd("C:/Users/Robin/Desktop/TDDE01")

## Setup ##

data=read.csv("optdigits.csv", header = F)
set.seed(12345)
ind <- sample(3, nrow(data), replace = TRUE, prob = c(0.5,0.25,0.25))
trdata <- data[ind==1,]
vdata <- data[ind==2,]
tedata <- data[ind==3,]

## TASK 2 ##

library(kknn)
tedata[, 65] <- as.factor(tedata[, 65])
data[, 65] <- as.factor(data[, 65])
trdata[, 65] <- as.factor(trdata[, 65])
vdata[, 65] <- as.factor(vdata[, 65])

nearest <- kknn(as.factor(V65)~.,train=trdata, test=tedata, k=30, kernel="rectangular")
table1 <- table(tedata[, 65], fitted.values(nearest))
missclass_error = 1 - sum(diag(table1)/sum(table1))

nearest1 <- kknn(as.factor(V65)~.,train=trdata, test=trdata, k=30, kernel="rectangular")
table2 <- table(trdata[, 65], fitted.values(nearest1))
missclass_error = 1 - sum(diag(table2)/sum(table2))

## TASK 3 ##

ottatrdata <- trdata$V65==8
probottatr <- nearest1$prob[,9]
## Use order to retrieve the easiest three and the hardest two 8s
order_higher <- order(probottatr,decreasing = TRUE)
order_higher <- order_higher[1:3]
order_lower <- order(probottatr,decreasing = FALSE)
order_lower <- order_lower[1:2]
order_Full <- c(order_higher,order_lower)

for (i in order_Full){
  numheatmaptrmax <- matrix(as.numeric(trdata[i,1:64]), nrow=8,ncol=8, byrow = T)
  
  heatmap(numheatmaptrmax, Colv = NA, Rowv = NA, )
}

nearest <- kknn(as.factor(V65)~.,train=trdata, test=tedata, k=6, kernel="rectangular")
table1 <- table(tedata[, 65], fitted.values(nearest))
missclass_error = 1 - sum(diag(table1)/sum(table1))

nearest1 <- kknn(as.factor(V65)~.,train=trdata, test=trdata, k=6, kernel="rectangular")
table2 <- table(trdata[, 65], fitted.values(nearest1))
missclass_error = 1 - sum(diag(table2)/sum(table2))

nearest2 <- kknn(as.factor(V65)~.,train=trdata, test=vdata, k=6, kernel="rectangular")
table3 <- table(vdata[, 65], fitted.values(nearest2))
missclass_error1 = 1 - sum(diag(table3)/sum(table3))

## TASK 4 ##
key <- c(1:30)
missclass_errortr <- numeric(30)
missclass_errorv <- numeric(30)
cross_entropy <- numeric(30)
for (i in key){
  nearest1 <- kknn(as.factor(V65)~.,train=trdata, test=trdata, k=i, kernel="rectangular")
  table2 <- table(trdata[, 65], fitted.values(nearest1))
  missclass_error = c(1 - sum(diag(table2)/sum(table2)))
  missclass_errortr[i] <-  missclass_error
  
  nearest2 <- kknn(as.factor(V65)~.,train=trdata, test=vdata, k=i, kernel="rectangular")
  table3 <- table(vdata[, 65], fitted.values(nearest2))
  missclass_error1 = c(1 - sum(diag(table3)/sum(table3)))
  missclass_errorv[i]<-  missclass_error1
  
  cross_entropy1 = - sum(c(1 - sum(diag(table3) * log(1e-15 + diag(table2)))))
  cross_entropy[i] <- cross_entropy1
}

plot(key, missclass_errortr, ylab = "missclassification_error", xlab = "k", col="blue")
points(key, missclass_errorv, col="red")

## TASK 5 ##

key <- c(1:30)
digits <- c(1:10)
cross_entropy <- numeric(30)

for (i in key){
  nearest1 <- kknn(as.factor(V65)~.,train=trdata, test=trdata, k=i, kernel="rectangular")
  table2 <- table(trdata[, 65], fitted.values(nearest1))
  temp = 0
  for (digit in digits){
    cross_entropy1 <- sum(nearest1$prob[,digit]*log(nearest1$prob[,digit] + 1e-15))
    temp <- c(temp,cross_entropy1)
  }
  cross_entropy[i] <- -sum(temp)
}

print(cross_entropy)
plot(key,cross_entropy, ylab = "cross_entropy_error", xlab = "k", col="blue")
plot(key,cross_entropy, col="red")
