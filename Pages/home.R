library(shiny)
library(shinydashboard)

path = "/home/leaveit/Documents/Data_Sci_Dashboard/Pages"
setwd(path)
source("customer.R")
path = "/home/leaveit/Documents/Data_Sci_Dashboard/Pages"
setwd(path)
source("orders_func.R")
path = "/home/leaveit/Documents/Data_Sci_Dashboard/Pages"
setwd(path)
source("shipping.R")





sidebar <- dashboardSidebar(
  sidebarMenu(
    id="tabs",
    menuItem("Customers", tabName="customers"),
    menuItem("Products", tabName = "products"),
    menuItem("Orders", tabName = "orders"),
    menuItem("Shipping", tabName = "shipping")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem("customers",
      h1("Customers"),
      fluidRow(width= 6, box(plotOutput("total.proportion.gender"))),
      fluidRow(
        box(selectInput("country", "Countries:", countries)),
        box(plotOutput("country.proportion.gender")),
        ),
      fluidRow(box(plotOutput("num.customers")))
      ),
    tabItem("products", 
        h1("Products"),
        h2("products")
      ),
    tabItem("orders",
        box(plotOutput("order.count"))      
      ),
    tabItem("shipping", 
      box(plotOutput("ship.cost.dist"))
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
}
  

shinyApp(ui, server)







