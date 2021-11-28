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

cov_mat = cov(df_scaled)
#heatmap(cov_mat)
ev = eigen(cov_mat)



num_vectors = dim(ev$vectors)

#for (n in 1:dim(ev$vectors)[1]){ #number of values to scale by
df_pc_n = data.frame()

for (n in 1:dim(df_scaled)[2]){
  for (row in 1:dim(df_scaled)[1]){

    eigenvector = ev$vectors[,n]
    
    df_pc_n[row,n] = t(as.matrix(df_scaled[row,])) %*% eigenvector #data.frame(as.matrix(df_scaled) %*% t(eigenvector))
  
  }
}
dim(df_pc_n)
plot(df_pc_n[,1:2], main ="Plot of first to components")

total_var = sum(diag(var(df_pc_n)))
cumsum_var = cumsum(diag(var(df_pc_n)))

features_required = which(c(cumsum_var) > 95)[1] #35 features needed, variance above 95% of variance

plot(cumsum_var, type="p",col="red", xlab = "number of used features", ylab="cumsum variance", main = "Cumsum variance ", sub =paste("number of features required to achiev 95% : ", features_required) )
lines(c(0,100),c(95,95),col="blue")

lines(c(features_required,features_required),c(0,100), col="blue")

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
# ---------------------------- TASK 2 done -------------------------------------
df = read.csv("lab2/communities.csv")
summary(df)

#scale and split 50/50
df = scale(df, TRUE, TRUE)

n <- dim(df)[1]
id <- sample(1:n,floor(n*0.5))
df_train <- data.frame(df[id,])
df_test <- data.frame(df[-id,])


m = lm(ViolentCrimesPerPop ~ .,df_train)
summary(m)
yhat = predict(m,df_test, type="response")

mse = c(mean(m$residuals^2), mean((yhat-df_test$ViolentCrimesPerPop)^2))
r2_test = 1 - sum((yhat-df_test$ViolentCrimesPerPop)^2) / sum((df_test$ViolentCrimesPerPop - rep(mean(df_test$ViolentCrimesPerPop),dim(df_test)[2]))^2) 
r2 = c(summary(m)$r.squared,r2_test)

r2_test
df_results = data.frame(cbind(mse,r2))
rownames(df_results) = c("Train", "Test")
df_results

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




sed.seed(12345)
optim_res = optim(par=theta0,fn=cost_func,  X_train = X_train ,y_train = y_train,X_test=X_test, y_test=y_test,method = "BFGS", control=list(trace=TRUE))


theta_best = optim_res$par

MSE_train_optim = optim_res$value 
MSE_test_optim = mean((y_test - (X_test %*% theta_best))^2)

optim_MSE = c(MSE_train_optim, MSE_test_optim)
colnames(df_results) = c("lm-MSE", "lm-R^2")
df_results = cbind(df_results,optim_MSE)

  
  

num_removed = 100
filtered_train_data = mse_train_vals[c(TRUE,rep(FALSE,num_removed))]
filtered_test_data = mse_test_vals[c(TRUE,rep(FALSE,num_removed))]

test_min_ind = which(mse_test_vals==min(mse_test_vals))



num_points = max(dim(as.matrix(filtered_train_data)))

plot(filtered_train_data,xlim=c(0,num_points), ylim=c(0,1.5), col = "blue", main="MSE for train(blue) and test(red) \n Lines are lowest achieved using PCA", xlab=paste("number of iterations divided by ",num_removed),ylab="Mean Square Error")
points(filtered_test_data,pch=21, col="red")
#adding linges from previous model
lines(c(0,225),rep(df_results[1,1],2),col="blue")
lines(c(0,225),rep(df_results[2,1],2),col="red")

print(df_results)

##we clearly see from both graph and the df that the two methods perform the same, because they are.


#------------------------------ TASK 4 done -------------------------------------

head(df_pc_n) ## df with pca applied
raw_df = data.frame(read.csv("lab2/communities.csv"))
head(raw_df)



df_join = cbind(df_pc_n,raw_df$ViolentCrimesPerPop)
colnames(df_join)[length(colnames(df_join))] = "ViolentCrimesPerPop"
head(df_join)


df_join = scale(df_join)

sed.seed(12345)
n <- dim(df_join)[1]
id <- sample(1:n,floor(n*0.5))
df_train <- data.frame(df_join[id,])
df_test <- data.frame(df_join[-id,])

dim(df_train)
dim(df_test)

m = lm(ViolentCrimesPerPop ~ ., df_train)
yhat =  predict(m,df_test, type="response")



MSE_PCA_train = mean(m$residuals^2)
MSE_PCA_test = mean((df_test$ViolentCrimesPerPop - yhat)^2)


r2_PCA_train = 1 - sum((m$fitted.values-df_train$ViolentCrimesPerPop)^2) / sum((df_train$ViolentCrimesPerPop - rep(mean(df_train$ViolentCrimesPerPop),dim(df_train)[1]))^2) 

r2_PCA_test = 1 - sum((yhat-df_test$ViolentCrimesPerPop)^2) / sum((df_test$ViolentCrimesPerPop - rep(mean(df_test$ViolentCrimesPerPop),dim(df_test)[1]))^2) 

df_results$PCA_MSE = c(MSE_PCA_train, MSE_PCA_train)
df_results$PCA_R2 = c(r2_PCA_train, r2_PCA_test)

df_results




