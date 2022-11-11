library(ggplot2)
library(dplyr)

path_data = "/home/leaveit/Documents/Dashboard_data_sci/data"
if (getwd() != path_data){
  setwd("/home/leaveit/Documents/Dashboard_data_sci/data")
}

product = read.csv("product_details.csv")
attach(product)

product.data.table = function(){
  datatable(head(product, 1000))
}

prod.category.List = unique(product.category)
prod.List = unique(product.name)

data = prod %>% select(product.name, product.price, product.discount) %>%
  group_by(product.name)


d = unique(data)


# least expensive, most expensive
# highest discount
# products offered in different categories










