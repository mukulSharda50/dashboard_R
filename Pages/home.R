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
      tabsetPanel(type = "tabs",
          tabPanel("Summary", verbatimTextOutput("trans.summary")),
          tabPanel("Plots", plotOutput("profit.plot")),
          tabPanel("Table", dataTableOutput("transaction.table"))
      ),
    ),
    tabItem("shipping",
      tabsetPanel(type = "tabs",
          tabPanel("Summary", verbatimTextOutput("ship.summary")),
          tabPanel("Plots", plotOutput("ship.cost.dist")),
          tabPanel("Table", dataTableOutput("ship.table"))
      ),
    ),
    tabItem("orders",
      tabsetPanel(type = "tabs",
          tabPanel("Summary", verbatimTextOutput("orders.summary")),
          tabPanel("Plots", plotOutput("order.count")),
          tabPanel("Table", dataTableOutput("order.table"))
      ),
    ),
    tabItem("products",
      tabsetPanel(type = "tabs",
          tabPanel("Summary", verbatimTextOutput("prod.summary")),
          tabPanel("Table", dataTableOutput("product.table"))
      )
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
  output$profit.plot = renderPlot(profit.plots())
  output$transaction.table = renderDataTable(trans.data.table())
  output$product.table = renderDataTable(product.data.table())
  output$order.table = renderDataTable(order.data.table())
  output$ship.table = renderDataTable(ship.data.table())
  output$trans.summary = renderPrint(trans.summ)
  output$prod.summary = renderPrint(trans.summ)
  output$order.summary = renderPrint(trans.summ)
  output$ship.summary = renderPrint(trans.summ)
}
  

shinyApp(ui, server)







