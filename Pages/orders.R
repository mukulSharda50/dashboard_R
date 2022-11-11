library(ggplot2)
library(dplyr)

path_data = "/home/leaveit/Documents/Dashboard_data_sci/data"
if (getwd() != path_data){
  setwd("/home/leaveit/Documents/Dashboard_data_sci/data")
}

order = read.csv("order_details.csv")
attach(order)

order.data.table = function(){
  datatable(head(order, 1000))
}

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









