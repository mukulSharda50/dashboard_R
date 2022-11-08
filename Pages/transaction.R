library(ggplot2)
library(dplyr)

path_data = "/home/leaveit/Documents/Data_Sci_Dashboard/data"
if (getwd() != path_data){
  setwd("/home/leaveit/Documents/Data_Sci_Dashboard/data")
}

transaction = read.csv("transaction_details.csv")
products = read.csv("product_details.csv")
orders = read.csv("order_details.csv")
attach(transaction)

# pre-processing
any(is.null(transaction))
qty = as.numeric(unique(transaction[qty.purchased != "" & qty.purchased != "abc" ,]$qty.purchased))
transaction[qty.purchased == "",]$qty.purchased = median(qty)
transaction[qty.purchased == "abc",]$qty.purchased = median(qty)

any(is.na(customer.id))

unique(profit)

trans.profit = substring(profit, 2)
trans.profit = trimws(trans.profit, which="right")
trans.profit = as.numeric(trans.profit)


# Total profit and total sale
total.profit = sum(trans.profit)


j = transaction %>%
  inner_join(products, by="product.id") %>%
  select(qty.purchased, product.price)

prod.price = substring(j$product.price, 2)
prod.price = trimws(prod.price, which="right")
prod.price = as.numeric(prod.price)

total.sales = sum(prod.price, na.rm = T)

# pruchase segment
unique(purchase.segment)

# max qty and min qty purchased
max.qty.purchased = max(transaction$qty.purchased)
min.qty.purchased = min(transaction$qty.purchased)

# which product category has highest profit
q = transaction %>%
  inner_join(products) %>%
    select(product.category, product.name, profit) %>%
      group_by(product.category)

maxprofit.category = unique(q[q$profit == max(profit), ])

# profit per day
q1 = transaction %>% 
  inner_join(orders) %>%
    select(profit, order.month) %>%
      group_by(order.month) %>%
        summarise()

unique(q1$order.date)

plot(q1)




