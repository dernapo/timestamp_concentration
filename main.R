####################################################
## Define a distribution coeficient that defines how
## distributed are the events
####################################################


## Load libraries ####
library(ggplot2)
library(dplyr)

## Create examples ####

a <- c("2015-01-01", "2016-01-01", "2017-01-01", "2018-01-01")
a <- as.Date(a, "%Y-%m-%d")

b <- c("2015-01-01", "2015-01-02", "2015-01-03", "2015-01-15")
b <- as.Date(b, "%Y-%m-%d")

c <- c("2015-01-01", "2015-01-02", "2017-01-03", "2017-07-15")
c <- as.Date(c, "%Y-%m-%d")

d <- c("2015-01-01", "2017-01-02", "2017-01-03", "2019-07-15")
d <- as.Date(d, "%Y-%m-%d")

e <- c("2015-01-01", "2015-01-01", "2015-01-03", "2019-07-15")
e <- as.Date(e, "%Y-%m-%d")

## functions ####

date_dif <- function(my_dates) {
  len <- length(my_dates)
  #output <- list()
  i <- 1
  
  for (i in 1:len-1){
    output[i] <- as.integer(my_dates[i+1] - my_dates[i])
    i <- i + 1
  }
  
  output <- as.integer(output)
  
  return(sd(output)/mean(output))
}
  
date_mad <- function(my_dates) {
  len <- length(my_dates)
  #output <- list()
  i <- 1
  
  for (i in 1:len-1){
    output[i] <- as.integer(my_dates[i+1] - my_dates[i])
    i <- i + 1
  }
  
  output <- as.integer(output)
  
  return(median(abs(output - median(output)))/median(output))
  
}
  

## test ####

lapply(list(a, b, c, d, e), date_mad)

lapply(list(a, b, c, d, e), date_dif)

data.frame(a = a, b = b, c = c, d = d, e = e) %>% 
  ggplot(aes(x=e, y = 1)) +
  geom_point()
