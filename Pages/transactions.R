# 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 github
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
ship = read.csv("shipping_details.csv")

# profit distribution
profit.dist = function(){
  hist(transaction$profit)
}
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

# plot(profit.ship.cost$profit~profit.ship.cost$qty.purchased) #is this correct?
plot(profit.ship.cost$profit, profit.ship.cost$ship.cost,
     xlab="Profit", ylab="Shipping Cost", main="Relationship between profit and shipping cost")

# affect of discount on qty purchased or profit
discount = product$product.discount
qty = transaction$qty.purchased
prof = transaction$profit 

cor(discount, qty)
cor(discount, prof)

# which product is bought max in which country
data = transaction %>%
  inner_join(customer) %>%
  inner_join(product) %>%
  select(product.name, product.category, qty.purchased, customer.country) %>%
  group_by(customer.country) %>%
  summarise(max=max(qty.purchased))

View(data)















