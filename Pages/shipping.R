library(ggplot2)
library(shiny)
library(shinydashboard)
library(dplyr)

path_data = "/home/leaveit/Documents/Data_Sci_Dashboard/data"
if (getwd() != path_data){
  setwd("/home/leaveit/Documents/Data_Sci_Dashboard/data")
}

ship = read.csv("shipping_details.csv")
attach(ship)

# Pre-processing
cost = substring(ship.cost, 2)
unique(cost)
cost = trimws(cost, which="right")
cost[cost == "est"] = median(as.numeric(cost), na.rm = T)

# Histogram of Shipping cost, showing distribution of the cost
ship.cost.dist = function(){
  cost = as.numeric(cost)
  hist(cost)
}

shipMode = unique(ship.mode)
shipMode
