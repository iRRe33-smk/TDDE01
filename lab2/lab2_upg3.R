#setup
set.seed(12345)

df = read.csv("lab2/communities.csv")
head(df)
#print(dim(df))
summary(df)
print(dim(df))

#---------------------setup complete-------------
df_copy = data.frame(df)
df_features = subset(df_copy, select=-c(ViolentCrimesPerPop))
head(df_features)

df_scaled = scale(df_features, TRUE, TRUE) #scaling
means = colMeans(df_scaled)

summary(df_scaled) #means are all 0

cov_mtx = cov(df_scaled)
ev = eigen(cov_mtx)

#PC_matrix = X * Eigenvectors
PC_mtx = as.matrix(df_scaled) %*% ev$vectors
df_pc = data.frame(PC_mtx) 

#renaming columns in df_pc
nums = seq(1:dim(PC_mtx)[2])
pcs = rep("PC",dim(PC_mtx)[2])
col_names = paste(pcs,nums,sep="")#"PC1", "PC2"...
colnames(df_pc) = col_names

#plotting first two components
plot(df_pc[,1:2], main ="Plot of first to components")

#variance and total variance of base features
total_var_features = sum(diag(var(df_scaled))) # var is diag in covariance mtx
cumsum_var_features = cumsum(diag(var(df_scaled)))
features_required = which(c(cumsum_var_features > 95))[1]
features_required #96 features required to achieve >95 % of variance

plot(cumsum_var_features, type="p",col="red", xlab = "number of used features", ylab="cumsum variance base features", main = "Cumsum variance features ", sub =paste("number of features required to achieve 95% : ", features_required),ylim =c(0,105) )
lines(c(0,100),c(95,95),col="blue")
lines(c(features_required,features_required),c(0,100), col="blue")




#variance and total variance of PCA components
total_var = sum(diag(var(df_pc))) # var is diag in covariance mtx
cumsum_var = cumsum(diag(var(df_pc)))
components_required = which(c(cumsum_var) > 95)[1] 
components_required #35 features needed, variance above 95% of variance

by_each = diag(var(df_pc))/total_var
first_two = by_each[1:2]
first_two #pct of variance in the two first = [25%, 17%]

plot(cumsum_var, type="p",col="red", xlab = "number of used components", ylab="cumsum variance", main = "Cumsum variance PCA components ", sub =paste("required to achieve 95% : ", components_required),ylim =c(0,105) )
lines(c(0,100),c(95,95),col="blue")
lines(c(components_required,components_required),c(0,100), col="blue")




#-------------------------- TASK 1 DONE ---------------------------
get_RGB_colors <- function(y){
  
  R = y*255
  G = (1-y)*255
  B = 0
  
  alpha = 255
  
  RGB = rgb(R,G,B,alpha,maxColorValue = 255)
  
}
res = princomp(df_scaled)
cumsum(res$sdev ^ 2) # confirm 35 features needed
colors = sapply(df$ViolentCrimesPerPop,get_RGB_colors)
plot(res$scores[,1:2],col=colors,sub="points on gradient between green and red. Red is high crime rate", main = "Score Plot,  ")

ev1 = res$loadings[,1]
ev1
ev1[order(abs(ev1),decreasing = TRUE)[1:5]]
"
-- medFamInc: median family income (differs from household income for non-family households) (numeric - decimal)

-- medIncome: median household income (numeric - decimal)

-- PctKids2Par: percentage of kids in family housing with two parents (numeric - decimal)

-- pctWInvInc: percentage of households with investment / rent income in 1989 (numeric - decimal)

-- PctPopUnderPov: percentage of people under the poverty level (numeric - decimal)

These features have the most impact on PC1, economic means and stable family realtionships seem to correlate negatively with  crime rate. Whereas poverty correlates positively with crime rate.
The plot also shows crime rates seem to be hiher when  PC1 increases.

"
plot(abs(ev1[order(abs(ev1),decreasing = TRUE)]),main="Contribution to PC1, absolute value. Ordered") #many features have notable contributions. There is no natural reason to only examine the top 5. Other than 5 being a nice number.
# ---------------------------- TASK 2 done -------------------------------------


df = read.csv("lab2/communities.csv") #reload the data.
summary(df)

#scale and split 50/50
df = scale(df, TRUE, TRUE)
set.seed(12345)

n <- dim(df)[1]
id <- sample(1:n,floor(n*0.5))
df_train <- data.frame(df[id,])
df_test <- data.frame(df[-id,])


m = lm(ViolentCrimesPerPop ~ .,df_train)
summary(m)
yhat = predict(m,df_test, type="response")

mse = c(mean(m$residuals^2), mean((yhat-df_test$ViolentCrimesPerPop)^2))

r2_test = 1 - sum((yhat-df_test$ViolentCrimesPerPop)^2) / sum((df_test$ViolentCrimesPerPop - rep(mean(df_test$ViolentCrimesPerPop),dim(df_test)[1]))^2) 
r2 = c(summary(m)$r.squared,r2_test)

r2_test
df_results = data.frame(cbind(mse,r2))
rownames(df_results) = c("Train", "Test")
df_results

'plot(df_train$ViolentCrimesPerPop, col="green", main = "train")
points(m$fitted.values, col ="red")

plot(df_test$ViolentCrimesPerPop, col="green", main ="test")
points(yhat, col ="red")'

# ---------------------------------- TASK3 done ---------------------------------------
#Y = X*theta
mse_train_vals <<- rep(0,0)
mse_test_vals <<- rep(0,0)

cost_func <- function(theta, X_train, y_train, X_test, y_test){
  y_train_hat = X_train %*% theta
  y_test_hat = X_test %*% theta
  
  
  mse_train = mean((y_train-y_train_hat)^2)
  mse_train_vals <<- append(mse_train_vals,mse_train)
  
  mse_test = mean((y_test - y_test_hat)^2)
  mse_test_vals <<- append(mse_test_vals,mse_test)
'  print(paste("mse_train",mse_train))
  print(paste("mse_test",mse_test))
'  
  return(mse_train)
}



X_train = as.matrix(df_train[,1:(dim(df_train)[2]-1)])
y_train = as.matrix(df_train[c('ViolentCrimesPerPop')])

X_test = as.matrix(df_test[,1:(dim(df_test)[2]-1)])
y_test = as.matrix(df_test[c('ViolentCrimesPerPop')])



theta0 =rep(0,dim(X_train)[2])
theta0 = as.matrix(theta0)




set.seed(12345)
optim_res = optim(par=theta0,fn=cost_func,  X_train = X_train ,y_train = y_train,X_test=X_test, y_test=y_test,method = "BFGS", control=list(trace=TRUE))


theta_best = optim_res$par
theta_best

MSE_train_optim = optim_res$value 
MSE_test_optim = mean((y_test - (X_test %*% theta_best))^2)

optim_MSE = c(MSE_train_optim, MSE_test_optim)
colnames(df_results) = c("lm-MSE", "lm-R^2")
df_results = cbind(df_results,optim_MSE)
df_results
  
  

num_removed = 99
filtered_train_data = mse_train_vals[c(TRUE,rep(FALSE,num_removed))]
filtered_test_data = mse_test_vals[c(TRUE,rep(FALSE,num_removed))]

test_min_ind = which(mse_test_vals==min(mse_test_vals))



num_points = max(dim(as.matrix(filtered_train_data)))

plot(filtered_train_data,xlim=c(0,num_points), ylim=c(0,1.5), col = "blue", main="MSE for train(blue) and test(red) \n Lines are lowest achieved using lm", xlab=paste("number of iterations divided by ",num_removed + 1),ylab="Mean Square Error")
points(filtered_test_data,pch=21, col="red")
#adding linges from previous model
lines(c(0,225),rep(df_results[1,1],2),col="blue")
lines(c(0,225),rep(df_results[2,1],2),col="red")

# print(df_results)

mean(m$coefficients/c(theta_best,.019))



plot(m$coefficients, col="red", main ="plotting coefficients against each other", ylab = "coef", xlab="feature", sub="optim-blue, lm-red")
points(c(0,theta_best), col="blue")

##we clearly see from both graph and the df that the two methods perform the same, because they are.


#------------------------------ TASK 4 done ---------------------------------------

