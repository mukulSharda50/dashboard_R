library(ggplot2)
library(dplyr)

path_data = "/home/leaveit/Documents/Dashboard_data_sci/data"
if (getwd() != path_data){
  setwd("/home/leaveit/Documents/Dashboard_data_sci/data")
}

ship = read.csv("shipping_details.csv")
attach(ship)

# Histogram of Shipping cost, showing distribution of the cost
ship.cost.dist = function(){
  cost = as.numeric(ship.cost)
  hist(cost, main="Distribution of shipping cost", col="maroon")
}

shipMode = unique(ship.mode)
shipMode
