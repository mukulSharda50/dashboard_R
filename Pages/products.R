library(ggplot2)
library(shiny)
library(shinydashboard)
library(dplyr)

path_data = "/home/leaveit/Documents/Data_Sci_Dashboard/data"
if (getwd() != path_data){
  setwd("/home/leaveit/Documents/Data_Sci_Dashboard/data")
}

product = read.csv("product_details.csv")
attach(product)

prod.category.List = unique(product.category)
prod.List = unique(product.name)

data = prod %>% select(product.name, product.price, product.discount) %>%
  group_by(product.name)


d = unique(data)


# least expensive, most expensive
# highest discount
# products offered in different categories










