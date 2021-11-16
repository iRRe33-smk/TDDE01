## TASK 1 ##
# Read data from .csv file and add into DataFrame, remove age,sex,subject id, test_time
setwd("C:/Users/Robin/Desktop/TDDE01")

Dataframe=read.csv("parkinsons.csv",sep=",")
df <- data.frame(Dataframe[c(5,7:22)]) # Only use the correct voice charactheristics.

# Now scale and split the data into a 60/40 ratio of train/test by sampling
df_scaled <- as.data.frame(scale(df))
n <- dim(df)[1]
set.seed(12345)
id <- sample(1:n,floor(n*0.6))
train <- df_scaled[id,]
test <- df_scaled[-id,]
## ----- Completed ----- ##

## TASK 2 ##
# Assume that the motor_UPDRS is normally distribitated and
# compute a linear regression model.
fit1 <- lm(motor_UPDRS ~ .,data=train)
summary(fit1)
prediction_train <- predict(fit1,train)
prediction_test <- predict(fit1,test)
# Estimate prediction for training- and test data, then calculate MSE error
diff1 <- (prediction_train-train[,'motor_UPDRS'])
diff2 <- (prediction_test-test[,'motor_UPDRS'])
mse_train <- sum(diff1^2)/dim(train)[1]
mse_test <- sum(diff2^2)/dim(test)[1]
print(paste("MSE for training data: ",mse_train))
print(paste("MSE for test data: ", mse_test))
## ----- Completed ----- ##

## TASK 3 ##
# (a)LOGLIKELIHOOD FUNCTION
# Returns the -Log-likelihood
loglikelihood <- function(theta,Y,sigma,X){
  X <- as.matrix(X) # Convert data to matrix
  n <- dim(X)[1]     # Get number of rows
  theta<-as.matrix(theta)

  loss <- sum( (X%*%theta-as.matrix(Y))^2 ) 
  myLoglik <- (-n*log(sigma^2)/2) - (n*log(2*pi)/2) -loss/(2*sigma^2)
  return (myLoglik)
}

# (b) RIDGE FUNCTION
ridge_function <- function(theta,Y,lambda,X){
  # uses function from (a)
  n<-dim(X)[2]
  sigma<-(theta[n+1])
  theta<-as.matrix(theta[1:n])
  myLoglik <- loglikelihood(theta=theta,Y=Y,sigma=sigma,X=X)
  ridge <- -myLoglik + lambda*sum(theta^2)
  return(ridge)
}
# (c) RIDGEOPT
# uses function from (b) to find the optimal theta and sigma for a given lambda
ridgeOpt <- function(lambda,X,Y,sigma){
  X <- as.matrix(X)
  n <- dim(X)[2]  # Number of columns
  sigma=1
  initTheta <- matrix(0,n) # Initial value for theta
  opt <- optim(par=c(initTheta,sigma),fn=ridge_function,lambda=lambda,Y=Y,X=as.matrix(X),method = "BFGS")
  return(opt)
}
# (d) DM
DF_function <- function(X,Y,lambda){
  # Uses function from (c) to find the Degree of Freedom of the returned solution
  X <- as.matrix(X)
  Y <- as.matrix(Y) # motor_UPDRS
  n <- dim(X)[2]    #Number of columns
  I <- diag(n)
  Xt <- t(X)   #Transpose X
  # Formula for X(X_t*X + lambda*I)^-1 * X_t*Y
  DF <- X%*%solve(Xt%*%X + lambda*I) %*% Xt
  DF <- sum(diag(DF))
  print(paste("Degree of freedom: ",DF))
  return(DF)
}
## ----- Completed ----- ##
# TASK 4
#opt = ridgeOpt(lambda=100,X=X)
compare <- function(X, theta,Y, sigma,lambda){
  n <- dim(X)[1]
  logl <- loglikelihood(X=X,Y=Y,theta=theta,sigma=sigma)
  #print(logl)
  DF <- DF_function(X=X,Y=Y,lambda=lambda)
  aic_value <- (2*DF - 2*logl)
  return(aic_value)
}
lambdas <- c(1,100,1000)
Xtrain<-as.matrix(train[2:17])
Ytrain<-as.matrix(train[1])
Xtest=as.matrix(test[2:17])
Ytest<-as.matrix(test[1])
#opt = ridgeOpt(lambda=1,X=Xtrain,Y=Ytrain)
for(lambda in lambdas){
#  sigma <-runif(1) #Noise variance is uniformly distributed
  opt <- ridgeOpt(lambda=lambda,Y=Ytrain,X=Xtrain)
  theta <- as.matrix(opt$par[1:16]) #Weights
  sigma<- opt$par[17]
  Xtheta <- Xtrain %*% theta
  trainMSE <- sum( (Xtheta - Ytrain)^2 )/dim(Xtrain)[1]
  testMSE <- sum( (Xtest%*%theta - Ytest)^2 )/dim(Xtest)[1]
  df <- compare(X=Xtrain,Y=Ytrain,theta=theta,sigma=sigma,lambda=lambda)
  print(paste("Lambda:",lambda))
  print(paste("TrainMSE:",trainMSE))
  print(paste("TestMSE:",testMSE))
  print(paste("AIC:",df))
  print("-----")
}
# Seems we get the best combination of AIC and MSE values for lambda = 100(0)?
