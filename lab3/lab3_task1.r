setwd("C:/Users/Robin/Desktop/TDDE01")

set.seed(1234567890)
library(geosphere)
stations <- read.csv("stations.csv")
temps <- read.csv("temps50k.csv")

our_date <- "2013-11-04" # The date to predict
## Filter the temp data ##
temps_filtered<-temps[temps$date < as.Date(our_date),]

## merge the temps data with the station data ##
st <- merge(stations,temps,by="station_number")

h_distance <- 100000 # These three values are up to the students
h_date <- 0.15
h_time <- 0.45
a <- 58.4274 # The point to predict (up to the students)
b <- 14.826
target <- c(a,b)


times <- c("04:00:00", "06:00:00","08:00:00","10:00:00",
           "12:00:00","14:00:00", "16:00:00","18:00:00",
           "20:00:00","22:00:00","24:00:00")
temp <- vector(length=length(times))

# Students’ code here
plot(temp, type="o")

### TASK 1, SUM OF THREE GAUSSIAN KERNELS ###



### TASK 2, PRODUCT OF THREE GAUSSIAN KERNELS ###


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
    # timestamps closer to the original. Kernel is symmetric so
    # sign doesnt matter.
    return( sin(pi*(h1-h2)/24) )
}
### dateDist ###
dateDist <- function(h1,h2){
    # Computes the relative distance between days
    # over a year. Using sine-function for this is
    # ideal (value ranges from 0 to 1), given its periodicity 
    # and that it rewards days closer to the original.
    return( sin(pi*(h1-h2)/365) )
} 


