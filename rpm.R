library(tidyquant)
options("getSymbols.warning4.0"=FALSE)
options("getSymbols.yahoo.warning"=FALSE)
# Downloading Apple price using quantmod

getSymbols("AAPL", from = '2017-01-01',
           to = "2018-03-01",warnings = FALSE,
           auto.assign = TRUE)

head(AAPL)

v_raw = AAPL$AAPL.Adjusted
head(v_raw)

v_scale = v_raw - mean(v_raw)
v_scale = v_scale / var(v_raw)[1]^.5

PAA <- function(series_normalized, reduction_factor){
  n = length(series_normalized)
  m = ceiling(n/reduction_factor)
  x = c(rep(0,m))
  k = reduction_factor
  
  for (i in 1:(m-1)){
    x[i] = (1/k) * sum(series_normalized[(k*(i-1) +1):(k*i)])
  }
  if ( ceiling(n/k) - floor(n/k) == 0){
    x[m] = (1/k) * sum(series_normalized[(k*(m-1) +1):(k*i)])
  }else{
    print("choose other K")
    x[m] = (1/(n-k*(m-1))) * sum( series_normalized[(k*(m-1) +1):n])
  }
  #print(paste(length(x), m, n))
  return(x)
}

rpm <- function(v,norm = TRUE){
  N = length(v)
  mat = matrix(rep(v,N),N,N,byrow = T)
  rpm = mat - t(mat)
  
  if (norm){
    rpm = (rpm - min(rpm) )/ ( max(rpm) - min(rpm)) * 255  
  }
  
  return(rpm)
  
}

ex_scaler <- function(v){
  return( (v - mean(v)) / var(v)[1]^.5 )
}

offset = 15
length_series_base = 50

for (i in seq(1,length(v_scale)-length_series_base-1,offset)){
    
  ts = v_scale[i:(i+length_series_base-1)]
  
  ts_vector = c(t(ts))
  plot(ts_vector, type="b", main = paste("scaled: ",i))
  
  paa_vector = PAA(ts_vector,1.4)
  
  #print(paa_vector)
  plot(paa_vector, type="b", main = paste("PAA: ",i))
  mat = rpm(paa_vector)
  
  #print(dim(mat))

  heatmap(mat, symm = TRUE, Rowv = NA, Colv = NA, main=paste("Heatmap-RPM: ",i))

}





"
preprocessing

raw_data -> scale ((x-mu) / sigma) ->  dimensionality reduction (Piecwise Aggregation Approximation ) -> transform into 2d ( relative position matrix) -> feed to network

"







#getting data
tickers = c("AAPL", "MSFT", "GOOG", "AMZN",  "FB", "NVDA", "BRK-B", "JPM", "V", "UNH", "HD", "JNJ", "WMT", "PG", "BAC") # 16 largest excluding TSLA
 
prices <- tq_get(tickers,
                 from = "2010-02-10",
                 to = "2020-02-10",
                 get = "stock.prices")
df_prices = data.frame(prices)

k=2
seq_len = ceiling(32*k)
offset = ceiling(10)

dat_mat = matrix(data = c(rep(0,seq_len)),ncol=seq_len,nrow=1)
for (t in tickers){
  print(t)
  dat = df_prices[df_prices$symbol == t,"adjusted"]
  for(i in seq(from=1,to= (length(dat)-seq_len-1) ,by=offset)){
    
    #dat_mat = rbind(dat_mat,dat[i:(i + seq_len-1)])
    dat_mat = rbind(dat_mat,dat[i:(i + seq_len+1)])
    
  }
  
}
dat_mat = as.matrix(dat_mat)
#dat_mat = dat_mat[-1,] # removing first row of zeroes


print(dim(dat_mat))


dat_mat_sc = t(apply(dat_mat,1 ,FUN = ex_scaler)) #scaling rowwise
dim(dat_mat_sc)

dat_mat_paa = t(apply(dat_mat,1,FUN=PAA, reduction_factor = k))

num_datapoints = dim(dat_mat)[1]



mat_mat = apply(dat_mat_paa,1,FUN=rpm, norm=FALSE)

dim(mat_mat)


finished_data = array(mat_mat, dim=c(seq_len/k,seq_len/k,dim(dat_mat)[1]))
dim(finished_data)

"plots 3 random samples throgh every step of preprocessing. "
for (i in sample(num_datapoints, 3)){
  plot(dat_mat[i,], main = paste("pre ",i))
  plot(dat_mat_sc[i,], main = paste("scale ",i))
  plot(dat_mat_paa[i,],main = paste("post ",i))
  heatmap(finished_data[,,i], symm=T, Rowv = NA, Colv = NA, main=paste("Heatmap-RPM: ",i))
}

"few examples of simple patterns and RPM plots, to understand how they work"
'
heatmap(array(c(rep(1,(32^2)/2),rep(0,(32^2)/2)),dim=c(32,32)),symm = T)

heatmap(rpm(1:32), symm=T, Rowv = NA, Colv = NA, main=paste("Heatmap-RPM constant increase: "))

heatmap(rpm(seq(32,1,-1)), symm=T, Rowv = NA, Colv = NA, main=paste("Heatmap-RPM constant decrease: "))

heatmap(rpm(c(rep(0,12),rep(1,12),rep(0,12))), symm=T, Rowv = NA, Colv = NA, main=paste("Heatmap-RPM 0-1-0: "))

heatmap(rpm(runif(32)), symm=T, Rowv = NA, Colv = NA, main=paste("Heatmap-RPM noise: "))

'


#training model



