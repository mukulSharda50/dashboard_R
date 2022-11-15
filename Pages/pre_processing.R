# Customer Dataset
customer = read.csv("customer_details.csv")
attach(customer)

if(any(is.na(customer) | is.null(customer))){
  print("Missing data in customer")
}
unique(customer.gender)
cust.city = unique(customer.city)
any(cust.city == "" | cust.city == "?")

cust.state = unique(customer.state)
any(cust.state == "" | cust.state == "?")

cust.country = unique(customer.country)
any(cust.country == "" | cust.country == "?")


cust.region = unique(customer.region)
any(cust.region == "" | cust.region == "?")

customer$customer.country = factor(customer.country)
customer$customer.region = factor(customer.region)
write.csv(customer, "customer_details.csv")

rm(customer)

# Order
order = read.csv("order_details.csv")
order$order.date = as.Date(order$order.date, "%m/%d/%y")
order$order.month = factor(order.month)
write.csv(order, "order_details.csv")
rm(order)


product = read.csv("product_details.csv")
unique(product$product.category)
unique(product$product.name)

product$product.price = as.numeric(substring(product$product.price, 2))
product[is.na(product$product.price), ]$product.price = median(product$product.price, na.rm=T)

unique(product$product.discount)
product$product.discount = as.numeric(product$product.discount)
product[is.na(product$product.discount), ]$product.discount = median(product$product.discount, na.rm = T) 
head(product)
write.csv(product, "product_details.csv")
rm(product)

# shipping
shipping = read.csv("shipping_details.csv")
shipping = shipping[, -2]
colnames(shipping)
shipping$ship.date = as.Date(shipping$ship.date, "%m/%d/%y")

unique(shipping$ship.mode)
shipping[shipping$ship.mode == "45788", ]$ship.mode = "Same Day"

shipping$ship.cost = as.numeric(trimws(substring(shipping$ship.cost, 2), which="right"))
shipping[is.na(shipping$ship.cost), ]$ship.cost = median(shipping$ship.cost, na.rm = T)


head(shipping)
write.csv(shipping, "shipping_details.csv")
rm(shipping)


# Transaction details
transaction = read.csv("transaction_details.csv")
head(transaction)
any(is.na(transaction) | is.null(transaction))

unique(transaction$qty.purchased)
transaction$qty.purchased =  as.numeric(transaction$qty.purchased)

transaction[is.na(transaction$qty.purchased), ]$qty.purchased = median(transaction$qty.purchased, na.rm=T) 

transaction$profit = as.numeric(trimws(substring(transaction$profit, 2), which="right"))
unique(transaction$purchase.segment)

transaction$purchase.segment = factor(transaction$purchase.segment, levels=c("Consumer", "Home Office" ,"Corporate"))

length(transaction[is.na(transaction$purchase.segment), ]) # 7

transaction[is.na(transaction$purchase.segment), ]$purchase.segment = "Home Office"


write.csv(transaction, "transaction_details.csv")
rm(transaction)
