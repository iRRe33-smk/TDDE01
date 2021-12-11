setwd("C:/Users/Robin/Desktop/TDDE01")

set.seed(1234567890)
library(geosphere)
stations <- read.csv("stations.csv")
temps <- read.csv("temps50k.csv")

our_date <- as.Date("2015-07-04") # The date to predict
## Filter the temp data ##
temps_filtered<-temps[temps$date < as.Date(our_date),]

## merge the temps data with the station data ##
st <- merge(stations,temps_filtered,by="station_number")

h_distance <- 1000 # These three values are up to the students
h_date <- 0.15
h_time <- 0.45
a <- 59.01850#58.4274 # The point to predict ((up to the students))This point is in Vadstena)
b <- 16.37090#14.826
## Target point ##
target_a <- t(rep(a,dim(st)[1]))
target_b <- t(rep(b,dim(st)[1]))
target <- matrix(c(a,b),ncol=2)
times <- c("04:00:00", "06:00:00","08:00:00","10:00:00",
           "12:00:00","14:00:00", "16:00:00","18:00:00",
           "20:00:00","22:00:00","24:00:00")
times_num <- c(4,6,8,10,12,14,16,18,20,22,24)
# Students’ code here
## Calculate contribution from relative distance
stationPos <- cbind(st$latitude,st$longitude)
k1 <- distHaversine(p1=target,p2=stationPos)
##Our functions ##

### Our Gaussian kernel ###
Gaussian_kernel <- function(deviation,scale_var){ 
  x <- deviation/scale_var
  return( exp(-x*x) )
}
### hourDist ###
hourDist <- function(h1,h2){
  # Computes the relative distance between hours
  # over a day Using sine-function for this is
  # ideal, given its periodicity and that it rewards
  # hours closer to the original. Kernel is symmetric so
  # sign doesnt matter.
  # Divide result with 3600 to get hours
  diff <- as.numeric( difftime(strptime(h1,format = "%H:%M:%S"), strptime(h2,format="%H:%M:%S")) )/3600
  return( sin(pi*(diff)/24) )
}
### dateDist ###
dateDist <- function(h1,h2){
  # Computes the relative distance between days
  # over a year. Using sine-function for this is
  # ideal (value ranges from 0 to 1), given its periodicity 
  # and that it rewards days closer to the original.
  diff <- as.Date(h1)-as.Date(h2)
  return( sin(pi*as.numeric(diff)/365) )
} 


### TASK 1, SUM OF THREE GAUSSIAN KERNELS ###

predicted_temp = rep(0,11)
predicted_temp2 = rep(0,11)
kernel_distance <- Gaussian_kernel(k1,h_distance)
kernel_days <- Gaussian_kernel(dateDist(temps_filtered$date,our_date),h_date)


## SUM OF KERNELS ##
for (i in 1:length(times)){
  kernel_time <- Gaussian_kernel(hourDist(times[i],temps_filtered$time),h_time)
  tmp <- as.matrix(kernel_distance + kernel_days + kernel_time)
  tmp <- (tmp/sum(tmp))*temps_filtered$air_temperature
  predicted_temp[i] <- sum(tmp)
  }
plot(times_num,predicted_temp,type="o",xlab = "Hours during the day",ylab = "Predicted temperature",main = "Predicted temperature using sum of three kernels")
### TASK 2, PRODUCT OF THREE GAUSSIAN KERNELS ###
#Product kernel#
for (i in 1:length(times)){
  kernel_time <- Gaussian_kernel(hourDist(times[i],temps_filtered$time),h_time)
  tmp <- as.matrix(kernel_distance*kernel_days*kernel_time)
  tmp <- (tmp/sum(tmp))*temps_filtered$air_temperature
  predicted_temp2[i] <- sum(tmp)
}
plot(times_num,predicted_temp2,type="o",xlab = "Hours during the day",ylab = "Predicted temperature",main = "Predicted temperature using product of three kernels")


