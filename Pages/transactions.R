library(ggplot2)
library(dplyr)

path_data = "/home/leaveit/Documents/Dashboard_data_sci/data"
if (getwd() != path_data){
  setwd("/home/leaveit/Documents/Dashboard_data_sci/data")
}

transaction = read.csv("transaction_details.csv")
product = read.csv("product_details.csv")
order = read.csv("order_details.csv")
customer = read.csv("customer_details.csv")

# profit distribution
hist(transaction$profit)

# total profit and total sale
total.profit = sum(transaction$profit)
prod.trans = transaction %>%
  inner_join(product, by="product.id") %>%
  select(qty.purchased, product.price)
total.sales = sum(prod.trans$qty.purchased * prod.trans$product.price, na.rm = T)
cat("total sales: ", total.sales)
cat("total profit: ", total.profit)


# purchase segment
unique(transaction$purchase.segment)


# max qty and min qty purchased
max.qty.purchased = max(transaction$qty.purchased)
min.qty.purchased = min(transaction$qty.purchased)


# which product category has highest profit
prod.profit = transaction %>%
  inner_join(product) %>%
    select(product.category, product.name, profit) %>%
      group_by(product.category)
maxprofit.category = unique(prod.profit[prod.profit$profit == max(prod.profit$profit), ])


# profit per month
profit.month = function(){
  q1 = transaction %>% 
    inner_join(order) %>%
      select(profit, order.month) %>%
        group_by(order.month) %>%
          summarise(sum=sum(profit))
  
  plot(q1$sum, main="Profit per month",type="l",
       xlab = "Months", ylab = "Profit")
}


# affect on profit when ship.cost is increased
profit.ship.cost = transaction %>%
  inner_join(ship) %>%
    select(profit, qty.purchased, ship.cost) %>%
      arrange(desc(ship.cost))
cor(profit.ship.cost)


# affect of discount on qty purchased or profit
discount = product$product.discount
qty = transaction$qty.purchased
prof = transaction$profit 

cor(discount, qty)
cor(discount, profit)

# which product is bought max in which region
data = transaction %>%
  inner_join(customer, by="customer.id") %>%
  inner_join(product, by="product.id") %>%
  select(product.category, product.name, qty.purchased, customer.state, 
         customer.country, customer.region) %>%
  filter(qty.purchased == max(qty.purchased) & customer.country == "India")


















