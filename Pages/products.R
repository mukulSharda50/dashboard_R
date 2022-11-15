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

unique.categ = length(unique(product.category))
unique.prod = length(unique(product.name))

prod.category.List = unique(product.category)
prod.List = unique(product.name)

data = product %>% select(product.name, product.price, product.discount) %>%
  group_by(product.name)


d = unique(data)

table(product.category)
total.product.count = product %>% count(product.name)
barplot(total.product.count$n,
        horiz = TRUE,
        names.arg = total.product.count$product.name)
