library(ggplot2)
library(dplyr)
library(plotly)
library(DT)

path_data = "/home/leaveit/Documents/Dashboard_data_sci/data"
if (getwd() != path_data){
  setwd("/home/leaveit/Documents/Dashboard_data_sci/data")
}

path = "/home/leaveit/Documents/Dashboard_data_sci/Pages"
setwd(path)
source("shipping.R")

transaction = read.csv("transaction_details.csv")
product = read.csv("product_details.csv")
order = read.csv("order_details.csv")
customer = read.csv("customer_details.csv")
ship = read.csv("shipping_details.csv")


# summary 
total.trans = length(unique(transaction$order.id))


# Plots
prof.month.line = function(){
  o = order
  o$order.month = factor(order$order.month, levels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                            "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
  profit.month = transaction %>% 
    inner_join(o, by="order.id") %>%
    select(profit, order.month) %>%
    group_by(order.month) %>%
    summarise(sum=sum(profit))
  
  #fig <- plot_ly(x=profit.month$order.month, y=profit.month$sum, name="profit", type="scatter", mode="lines")
  #fig <- fig %>% add_trace(y=d$sum, name="sales",line = list(color = 'rgb(22, 96, 167)', width = 4))
 
  
  plot_ly(profit.month, x = profit.month$order.month, 
          y = profit.month$sum, type = 'scatter', mode = 'lines+markers') %>%
    layout(xaxis=list(title="Months"), yaxis=list(title="Profits (in dollars)"))
}


prof.region.pie = function(){
  trans.cust.data = merge(transaction[, -c(1, 2)], customer[, -c(1, 2)], by="customer.id")
  profit.region = trans.cust.data %>%
    select(profit, customer.region) %>%
    group_by(customer.region) %>%
    summarise(sum = sum(profit))
  plot_ly(profit.region, labels=profit.region$customer.region, values=profit.region$sum, type="pie")
}

profit.per.category = function(){
  table = transaction %>% inner_join(product, by="product.id")
  ppc = tapply(table$profit, table$product.category, sum) 
  ppc = data.frame(ppc)
  plot_ly(ppc, labels=rownames(ppc), values=ppc$ppc, type="pie")
}

plot2 = function(){
  par(mfrow=c(2, 2))
  plot(density(transaction$profit), xlab="Profits",main="Bimodal Distribution of Profits", col="maroon")
  plot(density(transaction$quantity.purchased), col = "maroon", xlab="Quantity Purchased",
       main="Uniform Distribution of Quantity Purchased")
  ship.cost.dist()
  transaction[is.na(transaction$Aging), ]$Aging = median(transaction$Aging, na.rm=T)
  plot(density(transaction$Aging), col="maroon", main="Waiting time between product ordered and shipped",
       xlab="Waiting time (in days)")
}

plot3 = function(){
  age = scale(transaction$Aging)
  qty = scale(transaction$quantity.purchased)
  profsc = scale(transaction$profit)
  par(mfrow=c(2, 2))
  boxplot(data.frame(age, qty, profsc), main="Outlier Analysis")
  boxplot(transaction$Aging, main="Wait time for delivery of orders.")
  boxplot(transaction$profit, main="Profit")
}



# total profit and total sale
prod.trans = transaction %>%
  inner_join(product, by="product.id") %>%
  select(quantity.purchased, product.price)

total.sales = sum(prod.trans$quantity.purchased * prod.trans$product.price, na.rm = T)
total.profit = sum(transaction$profit)

# max qty and min qty purchased
max.qty.purchased = max(transaction$quantity.purchased)
min.qty.purchased = min(transaction$quantity.purchased)

# which product category has highest profit
prod.profit = transaction %>%
  inner_join(product, by="product.id") %>%
    select(product.category, product.name, profit) %>%
      group_by(product.category)
maxprofit.category = unique(prod.profit[prod.profit$profit == max(prod.profit$profit), ])

# affect on profit when ship.cost is increased
profit.ship.cost = transaction %>%
  inner_join(ship, by="ship.id") %>%
    select(profit, quantity.purchased, ship.cost) %>%
      arrange(desc(ship.cost))


# plot(profit.ship.cost$profit~profit.ship.cost$qty.purchased) #is this correct?
ship.qty = function() {
  plot(profit.ship.cost$profit, profit.ship.cost$ship.cost,
       xlab="Profit", ylab="Shipping Cost", 
       main="Relationship between profit and shipping cost")
}

# which product is bought max in which country
data = transaction %>%
  inner_join(customer, by="customer.id") %>%
  inner_join(product, by="product.id") %>%
  select(product.name, product.category, quantity.purchased, customer.country) %>%
  group_by(customer.country, product.category) %>% summarise(sum = sum(quantity.purchased))


# total num of products bought in each country
total.prod.country = function(){
  prod.purchase = transaction %>%
    inner_join(customer, by="customer.id") %>%
    inner_join(product, by="product.id") %>%
    select(product.name, product.category, quantity.purchased, customer.country) %>%
    group_by(customer.country) %>%
    summarise(sum=sum(quantity.purchased))
  
  plot_ly(prod.purchase, x=~prod.purchase$customer.country, y=~prod.purchase$sum, 
          type="scatter", mode="markers") %>%
    layout(xaxis=list(title="Countries"), yaxis=list(title="number of products"))
}

# Trending products in countries
trend.country = function(country){
  t = transaction[, -c(1, 2)]
  c = customer[, -c(1, 2)]
  p = product[, -c(1, 2)]
  data = merge(t, c)
  data = merge(data, p)
  
  trend = data %>% select(quantity.purchased, product.name, product.category, customer.country) %>%
    group_by(customer.country) %>%
    filter(customer.country == country & quantity.purchased == max(quantity.purchased))
 
  datatable(data.frame(unique(trend)))
}

avg.wait = function(){
  t = transaction[, -c(1, 2)]
  c = customer[, -c(1, 2)]
  data = t %>% inner_join(c, by="customer.id") %>% select(Aging, customer.country) %>%
    group_by(customer.country) %>% summarise(avg = mean(Aging))
  
  plot_ly(data, x=data$customer.country, y=data$avg, type="scatter")
}

sales = function(){
  t = transaction[, -c(1, 2)]
  c = customer[, -c(1, 2)]
  p = product[, -c(1, 2)]
  o = order
  o$order.month = factor(order$order.month, levels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                                     "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

  d = t %>% inner_join(c, by="customer.id") %>% inner_join(p, by="product.id") %>% 
    inner_join(o, by="order.id") %>% select(order.month, quantity.purchased, product.price) %>%
    mutate(sales = quantity.purchased*product.price) %>% group_by(order.month) %>%
    summarise(sum = sum(sales))
  
  plot_ly(d, x=d$order.month, y=d$sum, type="scatter", mode="lines+markers") %>%
    layout(xaxis=list(title="Months"), yaxis=list(title="Sales (in dollars)"))
}
shipcost = function(){
  t = transaction[, -c(1, 2)]
  o = order
  o$order.month = factor(order$order.month, levels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                                     "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
  s = t %>% inner_join(o, by="order.id") %>% inner_join(ship, by="ship.id") %>% 
    select(ship.cost, order.month) %>% 
    group_by(order.month) %>% summarise(sum = sum(ship.cost))
  plot_ly(s, x=s$order.month, y=s$sum, type="scatter", mode="lines+markers") %>%
    layout(xaxis=list(title="Months"), yaxis=list(title="Total shipping cost per month (in dollars)"))
}

# share of qty purchased of diff product categories in each country.
qty.category = function(country){
  table1 = transaction %>% inner_join(product, by="product.id") %>%
    inner_join(customer, by="customer.id") %>%
    select(profit, quantity.purchased, product.category, 
           product.name, customer.country) %>%
    group_by(customer.country, product.category) %>% 
    summarise(q=sum(quantity.purchased), p=sum(profit))
  
  d1 = table1[table1$customer.country == country, ]
  plot_ly(d1, labels=d1$product.category, values=d1$q, type="pie") %>%
    layout(title=list(text=paste("Showing chart for ", country), y = 0.05 ))
}


prod = unique(product$product.name)

prod.chart = function(prod.name){
  t = transaction %>% inner_join(product) %>% inner_join(customer)
  q = t %>% select(customer.country, product.name, quantity.purchased) %>% 
    group_by(product.name, customer.country) %>% summarise(sum=sum(quantity.purchased))
  t1 = q[q$product.name == prod.name, ]
  plot_ly(t1, x=~customer.country, y=~sum, type="bar", color=I("maroon")) %>%
    layout(title=list(text=paste("Top markets for ", prod.name), y=0.95))
}





