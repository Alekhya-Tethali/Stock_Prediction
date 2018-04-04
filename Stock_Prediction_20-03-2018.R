# put a message in console or server log; note this happens only when the app is started!
cat("Stock Market Analysis and Prediction started..\n")

# importing the shinydashboard package
library(shinydashboard)

# Sys.getenv obtains the values of the environment variables. skin is set to "" now. 
skin <- Sys.getenv("DASHBOARD_SKIN")
skin <- tolower(skin)
if (skin == "")
  skin <- "purple"

# A dashboard sidebar typically contains a sidebarMenu, although it may also contain 
# a sidebarSearchForm, or other Shiny inputs.
sidebar <- dashboardSidebar(
  sidebarSearchForm(label = "Search...", "searchText", "searchButton"),
  sidebarMenu(
    menuItem("Investment Advisor", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Dividends", icon = icon("th"), tabName = "dividends", badgeLabel = "new",
             badgeColor = "green"
    ),
    menuItem("Charts", icon = icon("bar-chart-o"),
             menuSubItem("Open charts", tabName = "subitem1"),
             menuSubItem("Close charts", tabName = "subitem2"),
             menuSubItem("High charts", tabName = "subitem3"),
             menuSubItem("Low charts", tabName = "subitem4")
    ),
    menuItem("Built By", tabName = "builtby", icon = icon("file-code-o")
             
             
             
    )
  )
)

body <- dashboardBody(
  tabItems(
    tabItem("dashboard",
            
            # Boxes with solid headers
            fluidRow(
              HTML("<center> <h1> <b> Investment Advisor </b> </h1> </center> <br>")
            ),
            fluidRow(
              box(
                title = "Enter Stock Code", width = 4, solidHeader = TRUE, status = "primary",
                textInput("StockCode", "StockCode:", value = "AAPL"),
                actionButton(inputId = "click", label = "Predict")
              )
              
              ,box(
                title = "Stock Price on",
                width = 4, solidHeader = TRUE, status = "primary",
                dateInput("Dates", "Date:", value = Sys.Date()),
                actionButton(inputId = "check", label = "Check") 
              )
              
              ,box(
                title = "Price", 
                width = 2, solidHeader = TRUE, status = "primary",
                height = 169,
                textOutput("stock.ondate")
              )
            ),
            fluidRow(
              
              box(
                title = "Auto Arima Plot",
                status = "primary",
                plotOutput("auto.arima", height = 350),
                height = 400
              ),
              box(
                title = "Auto Arima Table",
                
                width = 6,
                tableOutput("auto.arima1"),
                height = 400
              )
              
            )
    ),
    
    tabItem(tabName = "dividends",
            h2("Dividends"),
            tabBox(
              title = "Dividends", width = 15,
              # The id lets us use input$tabset1 on the server to find the current tab
              id = "tabset1", height = "400px",
              tabPanel(h3("What is dividend?"), 
                       "A dividend is defined as a payment made by a 
                       corporation to its shareholders. Usually these payouts are made in cash, 
                       called cash dividends, but sometimes companies will also distribute 
                       stock dividends, whereby additional stock shares are distributed to 
                       shareholders. Stock dividends are also known as stock splits."),
              
              
              tabPanel(h3("Quick Facts About Dividends"), 
                       HTML("1. The term dividend derives from the Latin term dividendum, or 
                            thing to be divided. In other words, companies divide their profits up among 
                            shareholders. <br/>
                            2. Companies have been paying dividends to shareholders for over 400 years. 
                            The first company to ever pay a dividend was the Dutch East India Company 
                            in the early 1600s.<br/>
                            3. Dividends alone have accounted for over 40% of the S&P 500's total returns 
                            since 1929.")
                       )
                       )
                       ),
    
    tabItem(tabName = "subitem1",
            fluidRow(
              HTML("<center> <h1> <b> Open Charts </b> </h1> </center> <br>")
            ),
            fluidRow(
              box(
                title = "Enter Stock Code", width = 4, solidHeader = TRUE, status = "primary",
                textInput("OpenStockCode", "StockCode:", value = "AAPL"),
                actionButton(inputId = "cbutton1", label = "Show Charts")
              ),
              
              fluidRow(
                box(
                  title = "Opening Prices:",
                  width = 11, solidHeader = TRUE, status = "primary",
                  plotOutput("openchart", height = 350),
                  height = 400
                )
              )
              
            )
    ),
    
    tabItem(tabName = "subitem2",
            fluidRow(
              HTML("<center> <h1> <b> Close Charts </b> </h1> </center> <br>")
            ),
            fluidRow(
              box(
                title = "Enter Stock Code", width = 4, solidHeader = TRUE, status = "primary",
                textInput("CloseStockCode", "StockCode:", value = "AAPL"),
                actionButton(inputId = "cbutton2", label = "Show Charts")
              ),
              
              fluidRow(
                
                box(
                  title = "Close Prices:",
                  width = 11, solidHeader = TRUE, status = "primary",
                  plotOutput("closechart", height = 350),
                  height = 400
                )
              )
              
            )
    ),
    
    tabItem(tabName = "subitem3",
            fluidRow(
              HTML("<center> <h1> <b> Close Charts </b> </h1> </center> <br>")
            ),
            fluidRow(
              box(
                title = "Enter Stock Code", width = 4, solidHeader = TRUE, status = "primary",
                textInput("HighStockCode", "StockCode:", value = "AAPL"),
                actionButton(inputId = "cbutton3", label = "Show Charts")
              ),
              
              fluidRow(
                
                box(
                  title = "High Prices:",
                  width = 11, solidHeader = TRUE, status = "primary",
                  plotOutput("highchart", height = 350),
                  height = 400
                )
              )
              
            )
    ),
    
    tabItem(tabName = "subitem4",
            fluidRow(
              HTML("<center> <h1> <b> Low Charts </b> </h1> </center> <br>")
            ),
            fluidRow(
              box(
                title = "Enter Stock Code", width = 4, solidHeader = TRUE, status = "primary",
                textInput("LowStockCode", "StockCode:", value = "AAPL"),
                actionButton(inputId = "cbutton4", label = "Show Charts")
              ),
              
              fluidRow(
                
                box(
                  title = "Low Prices:",
                  width = 11, solidHeader = TRUE, status = "primary",
                  plotOutput("lowchart", height = 350),
                  height = 400
                )
              )
              
            )
    ),
    
    tabItem(tabName = "builtby",
            
            h1("Guide"),
            h2("Dr. A. Sharada"),
            
            h1("Team members"),
            h2("Alekhya Tethali"),
            h2("Patlola Preethi"),
            h2("T. Vineela"),
            h2("Merugu Shanthi")
    )
                       )
              )

messages <- dropdownMenu(type = "messages",
                         messageItem(
                           from = "Yahoo finance",
                           message = "Dividend date of AAPL is on 24-03-2018."
                         ),
                         messageItem(
                           from = "New User",
                           message = "How to use the platform?",
                           icon = icon("question"),
                           time = "13:45"
                         ),
                         messageItem(
                           from = "Support",
                           message = "The new server is ready.",
                           icon = icon("life-ring"),
                           time = "2014-12-01"
                         )
)

notifications <- dropdownMenu(type = "notifications", badgeStatus = "warning",
                              notificationItem(
                                text = "5 new users today",
                                icon("users")
                              ),
                              notificationItem(
                                text = "BSE shares price hike",
                                icon("truck"),
                                status = "success"
                              ),
                              notificationItem(
                                text = "Server load at 86%",
                                icon = icon("exclamation-triangle"),
                                status = "warning"
                              )
)

tasks <- dropdownMenu(type = "tasks", badgeStatus = "success",
                      taskItem(value = 50, color = "green",
                               "Documentation"
                      ),
                      taskItem(value = 85, color = "aqua",
                               "Future values prediction"
                      ),
                      taskItem(value = 75, color = "yellow",
                               "Server deployment"
                      ),
                      taskItem(value = 40, color = "red",
                               "Overall project"
                      )
)

header <- dashboardHeader(
  title = "Investment Advisor", 
  messages,
  notifications,
  tasks
)

ui <- dashboardPage(header, sidebar, body, skin = skin)
uiOutput("pageStub")
server <- function(input, output) {
  
  
  #Stock Price on a particular date
  output$stock.ondate <- renderText(
    {
      
      print("entered output$stock.ondate")
      data_date <- eventReactive(input$check, {
        (input$Dates) 
      })
      ddate <- data_date
      
      data <- eventReactive(input$click, {
        (input$StockCode) 
      })
      Stock <- as.character(data())
      print(Stock)
      
      dayf = data.frame(date=Sys.Date()-1) 
      dayf$day <- weekdays(as.Date(dayf$date))
      dayf <- as.character(dayf)
      
      print("Conversion to weekdays finished")
      
      #confirming button click
      if(input$Dates==0)
      {
        return()
      } 
      
      if(dayf[2] == "Sunday" || is.null(dayf[2])) print("No stock exchanges on" + input$Dates)
      else{
        input$Dates
        Stock_date <- getSymbols(Symbols = Stock, src = "yahoo", env=NULL, 
                                 from = as.Date(input$Dates), to =  as.Date(input$Dates) + 1)
        
        Stock_on <- as.data.frame(Stock_date)
        print(Stock_on)
        
        paste("High Price:", 
              Stock_on[,2], 
              "Low Price:", 
              Stock_on[,3], 
              "Open Price:", 
              Stock_on[,1], 
              "Close Price:", 
              Stock_on[,4])
        
        
        #    Stock_on$Open = Stock_on[,1]
        #    paste("Opening price:", Stock_on$Open)
        #    Stock_on$High = Stock_on[,2]
        #    paste("High price:", Stock_on$High)
        #    Stock_on$Low = Stock_on[,3]
        #    paste("Low price:", Stock_on$Low)
        #    Stock_on$Close = Stock_on[,4]
        #    paste("CLosing price:", Stock_on$Close)
        #    Stock_on$Volume = Stock_on[,5]
        #    paste("Volume:", Stock_on$Volume)
        #    Stock_on$Adj = Stock_on[,6]
        #    paste("Adjusted price:", Stock_on$Adj)
        
      }
    }
  )
  
  
  
  #Auto.Arima - plot here
  output$auto.arima <- renderPlot({
    
    
    # if (is.null(input$StockCode))
    #   return()
    
    library('quantmod')
    library('ggplot2')
    library('forecast')
    library('tseries')
    
    #Stock <- as.character(input$StockCode)
    
    data <- eventReactive(input$click, {
      (input$StockCode) 
    })
    Stock <- as.character(data())
    print(Stock)
    Stock_df<-as.data.frame(getSymbols(Symbols = Stock, 
                                       src = "yahoo", from = "2017-01-01", env = NULL))
    Stock_df$Open = Stock_df[,1]
    Stock_df$High = Stock_df[,2]
    Stock_df$Low = Stock_df[,3]
    Stock_df$Close = Stock_df[,4] 
    Stock_df$Volume = Stock_df[,5]
    Stock_df$Adj = Stock_df[,6]
    Stock_df <- Stock_df[,c(7,8,9,10,11,12)] 
    
    
    
    #plot(as.ts(Stock_df$Close))
    
    Stock_df$v7_MA = ma(Stock_df$Close, order=7)
    Stock_df$v30_MA <- ma(Stock_df$Close, order=30)
    
    #STL
    rental_ma <- ts(na.omit(Stock_df$v7_MA), frequency=30)
    decomp_rental <- stl(rental_ma, s.window="periodic")
    #plot(decomp_rental)
    adj_rental <- seasadj(decomp_rental)
    #plot(adj_rental)
    
    
    #arima
    fit <- auto.arima(Stock_df$Close,ic="bic")
    fit.forecast <- forecast(fit)
    plot(fit.forecast,  main= Stock)
    fit.forecast
    
  })
  
  #Auto.Arima1 - plot here  Table output
  output$auto.arima1 <- renderTable({
    
    #if (is.null(input$StockCode))
    #  return()
    
    library('quantmod')
    library('ggplot2')
    library('forecast')
    library('tseries')
    
    #Stock <- as.character(input$StockCode)
    data <- eventReactive(input$click, {
      (input$StockCode)
    })
    
    
    Stock <- as.character(data())
    
    
    print(Stock)
    Stock_df<-as.data.frame(getSymbols(Symbols = Stock,
                                       src = "yahoo", from = "2017-01-01", env = NULL))
    Stock_df$Open = Stock_df[,1]
    Stock_df$High = Stock_df[,2]
    Stock_df$Low = Stock_df[,3]
    Stock_df$Close = Stock_df[,4]
    Stock_df$Volume = Stock_df[,5]
    Stock_df$Adj = Stock_df[,6]
    Stock_df <- Stock_df[,c(7,8,9,10,11,12)]
    
    #plot(as.ts(Stock_df$Close))
    
    Stock_df$v7_MA = ma(Stock_df$Close, order=7)
    Stock_df$v30_MA <- ma(Stock_df$Close, order=30)
    
    #STL
    rental_ma <- ts(na.omit(Stock_df$v7_MA), frequency=30)
    decomp_rental <- stl(rental_ma, s.window="periodic")
    #plot(decomp_rental)
    adj_rental <- seasadj(decomp_rental)
    #plot(adj_rental)
    
    
    #arima
    #auto.arima() Returns best ARIMA model according to either AIC, AICc or BIC value.
    fit <- auto.arima(Stock_df$Close,ic="bic")
    fit.forecast <- forecast(fit)
    #plot(fit.forecast,   col = "red")
    (fit.forecast)
  })
  
  
  #Open Charts - plot here
  output$openchart <- renderPlot({
    
    library('quantmod')
    library('ggplot2')
    library('forecast')
    library('tseries')
    
    dataopenc <- eventReactive(input$cbutton1, {
      (input$OpenStockCode)
    })
    
    Stockopenc <- as.character(dataopenc())
    
    print(Stockopenc)
    Stock_openc<-as.data.frame(getSymbols(Symbols = Stockopenc,
                                          src = "yahoo", from = "2017-01-01", to = "2018-03-01", env = NULL))
    Stock_openc$Open = Stock_openc[,1]
    
    plot(Stock_openc$Open, main = Stockopenc)
  })
  
  
  #Close Charts - plot here
  output$closechart <- renderPlot({
    
    library('quantmod')
    library('ggplot2')
    library('forecast')
    library('tseries')
    
    dataopenc <- eventReactive(input$cbutton2, {
      (input$CloseStockCode)
    })
    
    Stockclosec <- as.character(dataopenc())
    
    print(Stockclosec)
    Stock_closec<-as.data.frame(getSymbols(Symbols = Stockclosec,
                                           src = "yahoo", from = "2017-01-01", to = "2018-03-01", env = NULL))
    Stock_closec$Close = Stock_closec[,4]
    
    plot(Stock_closec$Close, main = Stockclosec)
  })
}

shinyApp(ui, server)
