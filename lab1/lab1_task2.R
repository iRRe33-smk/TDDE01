## TASK 1 ##
# Read data from .csv file and add into DataFrame, remove age,sex,subject id, test_time
setwd("C:/Users/Robin/Desktop/TDDE01")

Dataframe=read.csv("parkinsons.csv",sep=",")
df = data.frame(Dataframe[c(5,7:22)]) # Only use the correct voice charactheristics.

# Now scale and split the data into a 60/40 ratio of train/test by sampling
df_scaled = as.data.frame(scale(df))
n = dim(df)[1]
set.seed(12345)
id = sample(1:n,floor(n*0.6))
train = df_scaled[id,]
test = df_scaled[-id,]
## ----- Completed ----- ##

## TASK 2 ##
# Assume that the motor_UPDRS is normally distribitated and
# compute a linear regression model.
fit1=lm(motor_UPDRS ~ .,data=train)
summary(fit1)
pred_train = predict(fit1,train)
pred_test = predict(fit1,test)

# Estimate prediction for training- and testdata, then calculate MSE error
diff1 = (pred_train-train[1])
diff2 = (pred_test-test[1])
mse_train = sum(diff1*diff1)/dim(train)[1]
mse_test = sum(diff2*diff2)/dim(test)[1]
## ----- Completed ----- ##

## TASK 3 ##
# (a)LOGLIKELIHOOD FUNCTION
# Returns the -Log-likelihood
loglikelihood <- function(theta,Y,sigma,X){
  X <- as.matrix(X) # Convert data to matrix
  n <- length(X)[1]     # Get number of rows
  loss <- sum((X%*%theta-as.matrix(Y))^2) 
  myLoglik = 0.5*n*log(sigma)-0.5*n*log(2*pi)-loss/(2*sigma)
  return (myLoglik)
}

# (b) RIDGE FUNCTION
ridge_function <- function(theta,Y,sigma,lambda,X){
# uses function from (a)
  myLoglik <- loglikelihood(theta=theta,Y=Y,sigma=sigma,X=X)
  ridge <- -myLoglik + lambda*sum(theta^2)
  return(ridge)
}
# (c) RIDGEOPT
# uses function from (b) to find the optimal theta and sigma for a given lambda
ridgeOpt <- function(lambda,X,sigma){
  X <- as.matrix(X)
  Y <- as.matrix(X[,1])   # Assign Y = motor_UPDRS
  n <- dim(X)[2]
  
  initTheta <- as.matrix(rnorm(n)) # Initial value for theta
  opt <- optim(par=initTheta,fn=ridge_function,lambda=lambda,sigma=sigma,Y=Y,X=as.matrix(X),method = "BFGS")
  return(opt)
}
# (d) DM
DF_function <- function(X,lambda){
  # Uses function from (c) to find the Degree of Freedom of the returned solution
  X <- as.matrix(X)
  Y <- as.matrix(X[,1])
  n <- dim(X)[2]
  I <- diag(n)
  Xt <- t(X)
  # Formula for X(X_t*X + lambda*I)*X_t*Y
  DF <- X%*%solve(Xt%*%X + lambda*I) %*% Xt %*%Y
  DF <- sum(diag(DF))
  print(paste("Degree of freedom: ",DF))
  return(DF)
}
## ----- Completed ----- ##
# TASK 4
lambdas <- c(1,100,1000)
X=train
Xtest=as.matrix(test)
compare <- function(X, theta, sigma,lambda){
  Y <- as.matrix(X[,1])
  n <- dim(X)[1]
  logl <- loglikelihood(X=X,Y=Y, theta=theta,sigma=sigma)
  #print(logl)
  DF <- DF_function(X=X,lambda=lambda)
  aic_value <- (2*DF - 2*logl)/n
  return(aic_value)
}
for(lambda in lambdas){
  sigma <-runif(1) #Noise variance is uniformly distributed
  opt <- ridgeOpt(lambda=lambda,X=X,sigma=sigma)
  W <- as.matrix(opt$par) #Weights
  X <- as.matrix(X)
  Y <- as.matrix(X[,1])
  XW <-X %*% W
  trainMSE <- mean( (XW - Y[,1])^2 )
  testMSE<-mean( (Xtest%*%W - Xtest[,1])^2 )
  df <- compare(X=X,theta=W,sigma=sigma,lambda=lambda)
  print(paste("Lambda:",lambda))
  print(paste("TrainMSE:",trainMSE))
  print(paste("TestMSE:",testMSE))
  print(paste("AIC:",df))
  print("-----")
}
# Seems we get the best combination of AIC and MSE values for lambda = 100
