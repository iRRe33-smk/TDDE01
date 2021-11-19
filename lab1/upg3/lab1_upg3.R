setwd("tdde01-labbar/lab1")

df = read.csv2('pima-indians-diabetes.csv', sep=',', col.names = c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "y"))
head(df)
summary(df)
set.seed(12345)


#--------------------------------------- set-up complete ----------------------------------------------

make_colors <- function(x){
  if (x==1) {
    c = "red"
  }else{
    c ="green"
  }
  return(c)
}

colors = sapply(df$y, make_colors)
#task 1, it will not be easy. It is quite a messy relationship. The diabetes cases seem to come up almost everywhere. 
#There is no clear linear relationship between these 3 variables
plot(df$x2, df$x8,xlab="Plasma glucose concentration a 2 hours in an oral glucose tolerance test", ylab= "Age",main ="Plasma Glucose vs Age, colored indicates Diabetes", col=colors)

#----------------------------------------------------- task 1 Completed -------------------------------------
#creating model
m = glm(formula = "y~x2+x8", family = "binomial", data=df)
summary(m)


yhat = m$fitted.values
yhat[1:10] # yhat betweeen 0 and 1
summary(yhat) #mean is same as that of dataset


# returns 1(0) if p >=(<)r
prob_to_pred <- function(p, r=.5){

  if(p>=r){
    x=1
  }else {
    x=0
  }
  return(x)
}


ypred = sapply(yhat,prob_to_pred)
predcorr= cor(df$y,ypred)


#model coefficients W0, W2, W8
coefs = m$coefficients
coefs

#missclassification error
t = table(ypred,df$y)
missclass_error =  1 - sum(diag(t))/sum(t)
missclass_error



#plotting diabetes vs x2 and x8
#colors_true = sapply(df$y,make_colors)

colors_pred = sapply(ypred,make_colors)
plot(df$x2,df$x8,col=colors_pred, xlab = "Plasma glucose levels", ylab = "Age", main = paste("Model predictions for Diabetes \n Missclass_Error", toString(missclass_error), sep=" = ") ) 
# i still think this looks like a hard task. The models attempts to make a straight line to separate these the cases.


#probabalistic equation of estimator
# logistic([1, x2, x8] * m$coefficients)
'
df_ex = df
df_ex$model_applied = df_ex$constant * m$coefficients[1] + df_ex$x2 * m$coefficients[2] + df_ex$x8 * m$coefficients[3]

df_ex$constant = 1
logistic <- function(x){
  
  res = 1/(1+exp(-x))
}

df_ex$predicted = lapply(df_ex$model_applied, logistic)
df_ex$fitvals = m$fitted.values
head(df_ex)
'

#--------------------------------------- task 2 complete---------------------------------------------

#x2 = ( ln(-(r/(r-1))) - w8*x8 + w0 )/ w2

r=.5
w0 = coefs[1]
w2 = coefs[2]
w8 = coefs[3]

x8 = c(seq(19,85,0.1))
x2 = (log(-r/(r-1)) - w0 - w8*x8)/w2

plot(df$x2,df$x8,col=colors_pred, ylab = "Age", xlab= "Glucose Levels", main = paste("Model predictions for with decision boundry \n Missclass_Error", toString(missclass_error), sep=" = ")) 
lines(x2,x8,col="blue")



#------------------------------------- task 3 complete --------------------------------------------------------



for (r in c(.2,.5,.8)){
  w0 = coefs[1]
  w2 = coefs[2]
  w8 = coefs[3]
  
  x8 = c(seq(19,85,0.1))
  x2 = (log(-r/(r-1)) - w0 - w8*x8)/w2
  
  ypred_r = sapply(yhat,prob_to_pred, r=r)
  colors_with_r = sapply(ypred_r, make_colors)
  
  plot(df$x2,df$x8,col=colors_with_r, ylab = "Age", xlab= "Glucose Levels", main = paste("Model predictions for with decision boundry \n r", toString(r), sep=" == ")) 
  lines(x2,x8,col="blue")
}


#the model become more conservatime as R increases. r represenets the level of certainty required to predict positive.

#---------------------------------- task 4 complete -----------------------------------------------------------


df$z1 = df$x2 * df$x2 * df$x2 * df$x2
df$z2 = df$x2 * df$x2 * df$x2 * df$x8
df$z3 = df$x2 * df$x2 * df$x8 * df$x8
df$z4 = df$x2 * df$x8 * df$x8 * df$x8
df$z5 = df$x8 * df$x8 * df$x8 * df$x8
head(df)

m2 = glm("y~x2+x8+z1+z2+z3+z4+z5", family = "binomial", data=df)

summary(m2)
cor(df$y,m2$fitted.values)

yhat = m2$fitted.values
for (r in c(.2,.5,.8)) {
  ypred = sapply(yhat, prob_to_pred, r=r)
  colors = sapply(ypred,make_colors)
  
  t = table(ypred,df$y)
  missclass_error =  1 - sum(diag(t))/sum(t)
  missclass_error
  
  plot(df$x2,df$x8,col = colors,ylab = "Age", xlab= "Glucose Levels", main = paste("Model predictions with z-vars \n Missclassification rate ", toString(missclass_error),"\n r =",toString(r)))
  }

# function = logistic([1, x2, x8, z1 ,z2 ,z3 , z4, z5] * m$coefficients.T)
# The decision is no longer linear, as we have introduced non-linear variables. 
# THe missclassification rate is slightly lower, indicating better performance.
# AIC is also lower with bigger model. 802.5 -> 757.1 using r = .5



#-------------------------------- task 5 complete ----------------------------------------------------------------


