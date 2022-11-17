library(shiny)
library(shinydashboard)
library(DT)

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
          column(2, selectInput("products", "Select a product:", prod)),
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
      #fluidRow(
       # plotOutput("ship.qty.corr")
      #)
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






