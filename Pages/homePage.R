body <- dashboardBody(
  tabItems(
    tabItem("transaction",
            fluidRow(
              valueBox(total.trans, "Total Transactions", icon = shiny::icon("hashtag"), color = "teal"),
              valueBox(paste("$", total.profit), "Total Profit", icon = shiny::icon("bar-chart"), color = "teal"),
            ),
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
            fluidRow(
            ),
            tabsetPanel(type = "tabs",
                        tabPanel("Summary", verbatimTextOutput("prod.summary")),
                        tabPanel("Table", dataTableOutput("product.table"))
            )
    ),
    tabItem("customers",
            fluidPage(
              h1("Customers"),
              fluidRow(
                valueBox(cust.len, "Total number of customers", icon = icon("users"), color = "teal"),
                valueBox(cust.country, "Number of unique countries for our sales", icon = icon("globe"), color = "teal"),
              ),
              fluidRow(
                tabsetPanel(
                  type = "tabs",
                  tabPanel("Summary", verbatimTextOutput("cust.summ")),
                  tabPanel("Table", dataTableOutput("customer.table")),
                  
                ),
              ),
              fluidRow(
                column(3, plotOutput("total.proportion.gender")),
                column(9, plotOutput("num.customers"))
              ),
              fluidRow(
              ),
            )
    )
  )
)