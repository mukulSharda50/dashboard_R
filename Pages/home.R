library(shiny)
library(shinydashboard)

path = "/home/leaveit/Documents/Dashboard_data_sci/Pages"
setwd(path)
source("customers.R")
path = "/home/leaveit/Documents/Dashboard_data_sci/Pages"
setwd(path)
source("orders.R")
path = "/home/leaveit/Documents/Dashboard_data_sci/Pages"
setwd(path)
source("shipping.R")
path = "/home/leaveit/Documents/Dashboard_data_sci/Pages"
setwd(path)
source("transactions.R")


sidebar <- dashboardSidebar(
  sidebarMenu(
    id="tabs",
    menuItem("Customers", tabName="customers"),
    menuItem("Products", tabName = "products"),
    menuItem("Orders", tabName = "orders"),
    menuItem("Shipping", tabName = "shipping"),
    menuItem("Transaction", tabName = "transaction")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem("transaction", 
      box(plotOutput("profit.dist")),
      box(plotOutput("profit.month"))
    ),
    tabItem("shipping", 
      box(plotOutput("ship.cost.dist"))
    ),
    tabItem("orders",
      box(plotOutput("order.count"))      
    ),
    tabItem("products", 
      h1("Products"),
      h2("products")
    ),
    tabItem("customers",
      h1("Customers"),
      fluidRow(width= 6, box(plotOutput("total.proportion.gender"))),
      fluidRow(
        box(selectInput("country", "Countries:", countries)),
        box(plotOutput("country.proportion.gender")),
      ),
      fluidRow(box(plotOutput("num.customers")))
    )
  )
 )


ui <- dashboardPage(
  dashboardHeader(title = "Dashboard"),
  sidebar,
  body
)

server <- function(input, output){
  output$total.proportion.gender = renderPlot(total.proportion.gender)
  output$country.proportion.gender = renderPlot(country.proportion.gender(input$country))
  output$num.customers = renderPlot(num.customers())
  output$order.count = renderPlot(order.Months())
  output$ship.cost.dist = renderPlot(ship.cost.dist())
  output$profit.dist = renderPlot(profit.dist())
  output$profit.month = renderPlot(profit.month())
}
  

shinyApp(ui, server)







