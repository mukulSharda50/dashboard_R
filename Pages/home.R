library(shiny)
library(shinydashboard)
library(DT)

library(dplyr)
library(plotly)

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
    group_by(customer.country) %>% arrange(desc(quantity.purchased)) %>%
    filter(customer.country == country)
  
  datatable(data.frame(head(unique(trend), 15)))
  #plot_ly(trend, labels=~product.name, values=~quantity.purchased, type="pie")
  #plot_ly(trend, x=~product.name, y=~quantity.purchased, type="bar")
}
trend.country.prods = function(country){
  t = transaction[, -c(1, 2)]
  c = customer[, -c(1, 2)]
  p = product[, -c(1, 2)]
  data = merge(t, c)
  data = merge(data, p)
  
  trend = data %>% select(quantity.purchased, product.name, product.category, customer.country) %>%
    group_by(customer.country) %>% arrange(desc(quantity.purchased)) %>%
    filter(customer.country == country)
  plot_ly(trend, x=~product.name, y=~quantity.purchased, type="bar") %>%
    layout(xaxis=list(title=paste("Showing for ", country)), yaxis=list(title="number of products"))
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
    layout(title=list(text=paste("Top markets for ", prod.name), y=0.95), yaxis=list(title="Number of products bought"))
}

prod = function(){
  t = transaction %>% inner_join(customer, by="customer.id") %>% 
    inner_join(product, by="product.id") 
}


path_data = "/home/leaveit/Documents/Dashboard_data_sci/data"
if (getwd() != path_data){
  setwd("/home/leaveit/Documents/Dashboard_data_sci/data")
}

ship = read.csv("shipping_details.csv")
attach(ship)

# table
ship.data.table = function(){
  datatable(head(ship, 1000))
}

# Histogram of Shipping cost, showing distribution of the cost
ship.cost.dist = function(){
  cost = as.numeric(ship.cost)
  plot(density(cost), main="Distribution of shipping cost", col="maroon")
}

shipMode = unique(ship.mode)
tapply(ship$ship.cost, ship$ship.mode, median)



customer = read.csv("customer_details.csv")
attach(customer)

# Pre-processing
if (any(is.na(customer))){
  print("Yes")
}
gender = unique(customer.gender)
count.city = table(customer.city)
count.state = table(customer.state)
count.country = table(customer.country)
max.customers.country = sort(table(customer.country), decreasing = T)

customer.region = gsub("4orth", "North", customer.region)
customer.region = gsub("So3th", "South", customer.region)
count.region = table(customer.region)

cust.len = length(unique(customer.id))
cust.country = length(unique(customer.country))

cust.city = length(unique(customer.city))


customer.data.table = function(){
  datatable(head(customer, 1000))
}
cust.summ = summary(customer)
country = unique(customer.country)

# Visualization functions
# Pie chart of gender proportion in all regions.
data = as.data.frame(table(customer.gender))
proportion.gender <-ggplot(data, aes(x="", y="", fill=customer.gender)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() 

# Pie chart of gender proportion according to a country.
countries = unique(customer.country)
country.proportion.gender = function(country){
  data = as.data.frame(table(customer[customer.country == country, ]$customer.gender))
  piepercent<- round(100*data$Freq/sum(data$Freq), 1)
  pie(data$Freq, labels = piepercent, main = paste("gender in", country),col = rainbow(length(data$Freq)))
  legend("topright", c("Female","Male","Others"), cex = 0.8, fill = rainbow(length(data$Freq)))
}

# Bar Plot of number of customers in a region.
num.customers.region <- data.frame(table(customer.region))
x = 1:14
x = as.numeric(x)
num.customers <- function(){
  barplot(num.customers.region$Freq ~ x, 
          ylim = c(0, max(num.customers.region$Freq)+1),
          xlab="Regions",
          ylab="Number of customers",
          main = "Histogram of number of customers in a region",
          col = "darkred",
          names.arg = num.customers.region$customer.region
  )
}
highest.country.cust = data.frame(table(customer.country))
n = highest.country.cust$Freq

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

priority = function(){
  p = order[order.priority != "", ]$order.priority
  plot_ly(order, x=p)
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




sidebar <- dashboardSidebar(
  sidebarMenu(
    id="tabs",
    menuItem("Dashboard", tabName="dashboard")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem("dashboard",
            fluidRow(
              valueBox(cust.len,"Total Customers", color="red", icon=icon("users")),
              valueBox(cust.country, "Number of countries serving", icon=icon("globe")),
              valueBox(cust.city, "Number of cities serving", icon=icon("flag"), color="red")
            ),
            fluidRow(
              valueBox(paste("$",total.sales, sep = ""), "Total Sales", 
                       icon = icon("thumbs-up"), color = "red"),
              valueBox(paste("$",total.profit, sep = ""), "Total Profit", icon = icon("sack-dollar")),
              valueBox(total.trans, "Total Transactions till date", color="red", icon=icon("book"))
            ),
            br(),
            fluidRow(
              column(
                12,
                h3("Profit over 12 month period (Jan-Dec 2015):"),
                plotlyOutput("plot1"),
              ),
            ),
            br(),
            fluidRow(
              column(
                12,
                h3("Sales over 12 month period (Jan-Dec 2015):"),
                plotlyOutput("sales.mon")
              ),
            ),
            br(),
            fluidRow(
              column(12,
                     h3("Shipping cost over 12 month period (Jan-Dec 2015):"),
                     plotlyOutput("cost")
              )
            ),
            br(),
            br(),
            fluidRow(
              column(6,
                     h3("Percentage of customers in each country:"),
                     plotlyOutput("plot2", width="100%", height=500)
              ),
              column(6,
                     h3("Percentage share of product categories in profits:"),
                     plotlyOutput("profit.category", width="100%", height=500)
              )
            ),
            br(),
            fluidRow(
              column(6, 
                     h3("Total number of products bought in each country:"),
                     plotlyOutput("country.prod", height=571)
              ),
              column(6,
                     h3("Percentage Share in purchasing of different product categories in a country:"),
                     selectInput("country","Select a country:",country),
                     plotlyOutput("share.qty.prod", height=490)
              )
            ),
            br(),
            br(),
            fluidRow(
              column(
                4, 
                h3("Trending products in a country"),
                selectInput("countries", "Choose Country:", country),       
              ),
              column(
                8,
                h3("Output:"),
                dataTableOutput("trending")
              )
            ),
            br(),
            fluidRow(
              column(12,
                     h3("Top market countries for products offered:"),
                     column(2, selectInput("products", "Select a product:", prod.List)),
                     column(10, plotlyOutput("prodchart", height=600))
              )
            ),
            br(),
            fluidRow(
              column(12, 
                     h2("Statistical Distributions of attributes:"),
                     plotOutput("plot3", height=900),       
              )
            ),
            br(),
            fluidRow(
              column(12,
                     h2("Average Waiting Time for countries:"),
                     plotlyOutput("avg.waiting.time", height=600)       
              )
            ),
            br(),
            fluidRow(
              column(12,
                     h2("Priority of total orders:"),
                     plotlyOutput("order.prior", height=600)       
              )
            ),
            br(),
    )
  )
)

ui <- dashboardPage(
  dashboardHeader(title = "E-commerce Analytics"),
  sidebar,
  body
)

server <- function(input, output){
  output$plot1 = renderPlotly(prof.month.line())
  output$plot2 = renderPlotly(prof.region.pie())
  output$plot3 = renderPlot(plot2())
  output$plot4 = renderPlot(plot3())
  output$ship.qty.corr = renderPlot(ship.qty())
  output$country.prod = renderPlotly(total.prod.country())
  output$trending = renderDataTable(trend.country(input$countries))
  output$avg.waiting.time = renderPlotly(avg.wait())
  output$order.prior = renderPlotly(priority())
  output$sales.mon = renderPlotly(sales())
  output$cost = renderPlotly(shipcost())
  output$profit.category = renderPlotly(profit.per.category())
  output$share.qty.prod = renderPlotly(qty.category(input$country))
  output$prodchart = renderPlotly(prod.chart(input$products))
}

shinyApp(ui, server)