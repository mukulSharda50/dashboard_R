library(ggplot2)
library(dplyr)

path_data = "/home/leaveit/Documents/Dashboard_data_sci/data"
if (getwd() != path_data){
  setwd("/home/leaveit/Documents/Dashboard_data_sci/data")
}

ship = read.csv("shipping_details.csv")
attach(ship)

# table
ship.data.table = function(){
  datatable(head(ship, 1000))
}

# Histogram of Shipping cost, showing distribution of the cost
ship.cost.dist = function(){
  cost = as.numeric(ship.cost)
  plot(density(cost), main="Distribution of shipping cost", col="maroon")
}

shipMode = unique(ship.mode)
tapply(ship$ship.cost, ship$ship.mode, median)
