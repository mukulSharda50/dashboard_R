library(ggplot2)
library(shiny)
library(shinydashboard)
library(dplyr)

path_data = "/home/leaveit/Documents/Data_Sci_Dashboard/data"
if (getwd() != path_data){
  setwd("/home/leaveit/Documents/Data_Sci_Dashboard/data")
}

order = read.csv("order_details.csv")
attach(order)

priority = unique(order.priority)

order.Months <- function(){
months = factor(order.month, levels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                      "Jul",  "Aug", "Sep", "Oct", "Nov","Dec"))
count = table(months)
  plot(months, ylim=c(0, 5500), col="maroon", 
     xlab="Months", ylab="Count", main="Order Count in different months")
  abline(h = max(count))
}


# total sales









