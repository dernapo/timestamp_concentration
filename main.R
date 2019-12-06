####################################################
## Define a distribution coeficient that defines how
## distributed are the events
####################################################


## Load libraries ####
library(ggplot2)
library(dplyr)
library(data.table)
library(ineq)

## Create examples ####

a <- c("2015-01-01", "2016-01-01", "2017-01-01", "2018-01-01")
a <- as.Date(a, "%Y-%m-%d")
a_dt <- data.table(serie = "a", dates = a)

b <- c("2015-01-01", "2015-01-02", "2015-01-03", "2015-01-04")
b <- as.Date(b, "%Y-%m-%d")
b_dt <- data.table(serie = "b", dates = b)


c <- c("2015-01-01", "2015-01-02", "2017-01-03", "2017-07-15")
c <- as.Date(c, "%Y-%m-%d")
c_dt <- data.table(serie = "c", dates = c)

d <- c("2015-01-01", "2017-01-02", "2017-01-03", "2019-07-15")
d <- as.Date(d, "%Y-%m-%d")
d_dt <- data.table(serie = "d", dates = d)


e <- c("2015-01-01", "2015-01-01", "2015-01-03", "2019-07-15")
e <- as.Date(e, "%Y-%m-%d")
e_dt <- data.table(serie = "e", dates = e)

examples_dt <- data.table::rbindlist(list(a_dt, b_dt, c_dt, d_dt))


## Functions ####

date_distance <- function(my_dates) {
  my_dates <- sort(my_dates)
  len <- length(my_dates)
  output <- list()
  i <- 1
  
  for (i in 1:len-1){
    output[i] <- as.integer(my_dates[i+1] - my_dates[i])
    i <- i + 1
  }

  output <- as.integer(output)
  
  return(output)
} ## Distance between dates, first with second, second with third...

date_distance2 <- function(my_dates) {
  
  min_date <- min(my_dates)
  output <- my_dates[-1] - min_date
  
  
  output <- as.integer(output)
  
  return(output)
} ## Distance from the lower date

date_dif <- function(my_dates) {
  output <- date_distance(my_dates)
  return(sd(output)/mean(output))
}
  
date_mad <- function(my_dates) {
  output <- date_distance(my_dates)
  return(median(abs(output - median(output)))/median(output))
}

date_avg <- function(my_dates) {
  output <- date_distance(my_dates)
  return(mean(output))
}

date_30day <- function(my_dates) {

  n_dates <- length(my_dates)
  
  day_groups <- as.integer(cut(my_dates, seq(from = Sys.Date() - 10*365, to = Sys.Date(), by = 30)))
  
  n_groups <- length(unique(day_groups))
  
  return((n_groups-1)/(n_dates-1))

}
  
date_gini <- function(my_dates) {
  
  output <- date_distance(my_dates)
  return(ineq(output,type="Gini"))
  
}

## Test and visualization ####

summary_dt <- examples_dt[, .(mad = date_mad(dates), 
                              dif = date_dif(dates), 
                              avg = date_avg(dates),
                              group_30days = date_30day(dates),
                              gini = date_gini(dates)),
                          by = serie]



examples_dt %>% 
  ggplot(aes(x = serie, y = dates, color = serie)) +
  #geom_point() +
  geom_jitter(width = 0.1) +
  geom_text(data = summary_dt, aes(x = serie, 
                                   y = as.Date("2021-01-01", "%Y-%m-%d"), 
                                   label = paste0("mad: ", round(mad,2), 
                                                  "\navg: ", round(avg, 2), 
                                                  "\ndif: ", round(dif, 2),
                                                  "\ngini: ", round(gini, 2),
                                                  "\ngroup_30days: ", round(group_30days, 2)))) +
  ylim(as.Date("2014-01-01", "%Y-%m-%d"), as.Date("2022-01-01", "%Y-%m-%d")) +
  theme_set(theme_light()) +
  labs(title = "Dispersion measures of different date series examples (distance from lower date)",
       subtitle = "Which one should we use for domain ordering dispersion?")


