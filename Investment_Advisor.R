# put a message in console or server log; note this happens only when the app is started!
cat("Stock Market Analysis and Prediction started..\n")

# importing the shiny, shinydashboard packages for the web application UI
library(shinydashboard)
library(shiny)
#importing quantmod for getSymbols, getDividend
library(quantmod)
#importing ggplot2 for plotting the Arima graphs
library(ggplot2)
#importing forecast and tseries for ARIMA calculation
library(forecast)
library(tseries)
#importing data.table for setting the getDividends table - setting rowheads as 1st column data
library(data.table)
#importing for database 
require(RPostgreSQL)
library(DBI)
library(dbConnect)


fields <- c("username", "password")


messages <- dropdownMenu(
    type = "messages",
    messageItem(from = "Yahoo finance",
                message = "Dividend date of AAPL is on 24-03-2018."),
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

notifications <- dropdownMenu(
    type = "notifications",
    badgeStatus = "warning",
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

tasks <- dropdownMenu(
    type = "tasks",
    badgeStatus = "success",
    taskItem(
        value = 50, 
        color = "green",
        "Documentation"
    ),
    taskItem(
        value = 85, 
        color = "aqua",
        "Future values prediction"
    ),
    taskItem(
        value = 75, 
        color = "yellow",
        "Server deployment"
    ),
    taskItem(
        value = 40, 
        color = "red",
        "Overall project"
    )
)

header <- dashboardHeader(
    title = "Investment Advisor",
    messages,
    notifications,
    tasks
)

sidebar <- dashboardSidebar(  uiOutput("sidebarpanel"),
                              tags$iframe(align="right",
                                          width="230",
                                          height="300",
                                          src = "https://console.dialogflow.com/api-client/demo/embedded/a0610249-fbbf-4713-81c4-6c7fa590ded5", seamless=NA
                              ))  
body <- dashboardBody(uiOutput("body"))


ui <- dashboardPage(header, sidebar, body)


loginpart <- box(
    width = NULL,
    height = NULL,
    tabsetPanel(tabPanel(h3("Login"),
                         tags$div(
                             tags$p(login <- box(
                                 title = "Login",
                                 textInput("userName", "Username"),
                                 passwordInput("passwd", "Password"),
                                 br(),
                                 actionButton("Login", "Log in")
                             ))
                         )),
                
                
                tabPanel(h3("Sign Up"),
                         tags$div(
                             tags$p(signup <- box(
                                 textInput("username", "Username", ""),
                                 passwordInput("password", "Password", ""),
                                 br(),
                                 actionButton("Signup", "Sign Up")
                             ))
                         )))
)

server <- function(input, output, session) {
    # To logout back to login page
    loginpart.page = paste(
        isolate(session$clientData$url_protocol),
        "//",
        isolate(session$clientData$url_hostname),
        ":",
        isolate(session$clientData$url_port),
        sep = ""
    )
    histdata <- rnorm(500)
    USER <- reactiveValues(Logged = F)
    observe({
        if (USER$Logged == FALSE) {
            if (!is.null(input$Login)) {
                if (input$Login > 0) {
                    
                    
                    driver <- dbDriver("PostgreSQL")
                    
                    con <- dbConnect(driver, dbname = "miniproject",
                                     host = "localhost", port = 5432,
                                     user = "postgres", password = "Alekhya")
                    
                    my_username=dbGetQuery(con, "SELECT username FROM login;")
                    my_password=dbGetQuery(con, "SELECT password FROM login;")
                    print(my_username)
                    print(my_password)
                    
                    Username <- isolate(input$userName)
                    Password <- isolate(input$passwd)
                    Id.username <- which(my_username == Username)
                    Id.password <- which(my_password == Password)
                    print(Id.username)
                    print(Id.password)
                    if (length(Id.username) > 0 & length(Id.password) > 0) {
                        if (Id.username == Id.password) {
                            USER$Logged <- TRUE
                        }
                    }
                    else {
                        showModal(modalDialog(
                            title = "Unsuccessful Login",
                            paste0("Please check your credentials!"),
                            easyClose = TRUE,
                            footer = NULL
                        ))
                    }
                    dbDisconnect(con)
                }
            }
        }
        
    })
    
    output$sidebarpanel <- renderUI({
        if (USER$Logged == TRUE) {
            div(
                sidebarUserPanel(
                    isolate(input$userName),
                    subtitle = a(icon("usr"), "Logout", href = loginpart.page)
                ),
                sidebarMenu(
                    menuItem(
                        "Investment Advisor",
                        tabName = "dashboard",
                        icon = icon("dashboard")
                    ),
                    menuItem(
                        "Dividends",
                        icon = icon("th"),
                        tabName = "dividends"
                    ),
                    menuItem(
                        "Charts",
                        icon = icon("bar-chart-o"),
                        menuSubItem("Open charts", tabName = "open_charts_subitem"),
                        menuSubItem("High charts", tabName = "high_charts_subitem"),
                        menuSubItem("Low charts", tabName = "low_charts_subitem"),
                        menuSubItem("Close charts", tabName = "close_charts_subitem")
                        
                    ),
                    
                    menuItem(
                        "CAPM Advisor",
                        icon = icon("fa fa-list-ol"),
                        tabName = "Advisor_Table",
                        badgeLabel = "new",
                        badgeColor = "green"
                    ),
                    
                    menuItem(
                        "Built By",
                        tabName = "builtby",
                        icon = icon("file-code-o")
                    )
                )
            )
        }
    })
    
    output$body <- renderUI({
        if (USER$Logged == TRUE) {
            tabItems(
                #Investment Advisor
                tabItem(
                    "dashboard",
                    # Boxes with solid headers
                    fluidRow(
                        HTML("<center> <h1> <b> Investment Advisor </b> </h1> </center> <br>"),
                        tags$head(
                            tags$style(
                                HTML('body {background-color: lightblue;}')
                            )
                        )
                    ),
                    
                    fluidRow(
                        box(
                            title = "Enter Stock Code",
                            width = 4,
                            solidHeader = TRUE,
                            status = "primary",
                            textInput("StockCode", "StockCode:", value = "AAPL"),
                            actionButton(inputId = "click", label = "Predict")
                        ),
                        box(
                            title = "Stock Price on",
                            width = 4,
                            solidHeader = TRUE,
                            status = "primary",
                            dateInput("Dates", "Date:", value = Sys.Date()),
                            actionButton(inputId = "check", label = "Check")
                        ),
                        
                        box(
                            title = "Price",
                            width = 4,
                            solidHeader = TRUE,
                            status = "primary",
                            height = 169,
                            htmlOutput("stock.ondate")
                        )
                        
                    ),
                    
                    fluidRow(
                        box(
                            title = "Auto Arima",
                            status = "primary",
                            plotOutput("auto.arima", height = 350),
                            height = 400
                        ),
                        
                        box(
                            title = "Auto Arima",
                            width = 6,
                            tableOutput("auto.arima1"),
                            height = 400
                        )
                    )
                ),
                
                #Dividends tab
                tabItem(
                    tabName = "dividends",
                    
                    fluidRow(
                        HTML("<center> <h1> <b> Dividends </b> </h1> </center> <br>"),
                        tags$head(
                            tags$style(
                                HTML(
                                    "
                                    @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
                                    @import url('//fonts.googleapis.com/css?family=Georgia');
                                    h1 {
                                    font-family: 'Lobster', cursive;
                                    font-weight: 500;
                                    line-height: 1.5;
                                    color: #000099;
                                    }
                                    
                                    "
                                )
                                )
                                )
                                ),
                    
                    fluidRow(
                        tabBox(
                            title = "",
                            #don't give titile- UI is not good with title
                            width = 15,
                            # The id lets us use input$tabset1 on the server to find the current tab
                            id = "dividend_tabset1",
                            height = "250px",
                            tabPanel(
                                h3("What is dividend?"),
                                tags$div(
                                    tags$p(
                                        "A dividend is defined as a payment made by a corporation to its shareholders. Usually these payouts are made in cash, called cash dividends, but sometimes companies will also distributestock dividends, whereby additional stock shares are distributed to shareholders. Stock dividends are also known as stock splits.",
                                        style = "font-family: 'Georgia';
                                        font-weight: 1000; line-height: 1.5; font-size: 18px;
                                        color: #4d3a7d;"
                                    )
                                )
                            ),
                            
                            tabPanel(
                                h3("Quick Facts About Dividends"),
                                tags$div(
                                    tags$ul(
                                        tags$li(
                                            tags$p(
                                                "The term dividend derives from the Latin term dividendum, or
                                                thing to be divided. In other words, companies divide their profits up among
                                                shareholders.",
                                                style = "font-family: 'Georgia';
                                                font-weight: 1000; line-height: 1.5; font-size: 18px;
                                                color: #4d3a7d;"
                                            )
                                        ),
                                        
                                        tags$li(
                                            tags$p(
                                                "Companies have been paying dividends to shareholders for over 400 years.
                                                The first company to ever pay a dividend was the Dutch East India Company
                                                in the early 1600s.",
                                                style = "font-family: 'Georgia';
                                                font-weight: 1000; line-height: 1.5; font-size: 18px;
                                                color: #4d3a7d;"
                                            )
                                        ),
                                        
                                        tags$li(
                                            tags$p(
                                                "Dividends alone have accounted for over 40% of the S&P 500's total returns
                                                since 1929.",
                                                style = "font-family: 'Georgia';
                                                font-weight: 1000; line-height: 1.5; font-size: 18px;
                                                color: #4d3a7d;"
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    ),
                    
                    #dividends table
                    fluidRow(
                        box(
                            title = "Enter Stock Code",
                            width = 4,
                            solidHeader = TRUE,
                            status = "primary",
                            textInput("Dividend_StockCode", "StockCode:", value = "AAPL"),
                            actionButton(inputId = "dividend_click", label = "Go")
                        ),
                        
                        box(
                            title = "Previous Dividend Dates",
                            status = "primary",
                            width = 3,
                            tableOutput("dividend_display_table"),
                            height = 400
                        )
                    )
                                ),
                
                #Open Chart
                tabItem(
                    tabName = "open_charts_subitem",
                    fluidRow(
                        HTML("<center> <h1> <b> Open Charts </b> </h1> </center> <br>")
                    ),
                    
                    fluidRow(
                        box(
                            title = "Set Parameters",
                            width = 3,
                            solidHeader = TRUE,
                            status = "primary",
                            height = 500,
                            
                            textInput("OpenStockCode", "StockCode:", value = "AAPL"),
                            
                            dateRangeInput(
                                "OpenDates",
                                label = tags$div(tags$b("Date Range:")),
                                format = "dd-mm-yy",
                                start = Sys.Date() - 2000,
                                end = Sys.Date()
                            ),
                            
                            radioButtons(
                                inputId = "OpenPeriod",
                                label = tags$div(tags$b("Periodicity:")),
                                choices = c("daily", "weekly", "monthly")
                            ),
                            
                            radioButtons(
                                inputId = "OpenTimePeriod",
                                label = tags$div(tags$b("Time:")),
                                choices = c("last 1 year", "last 3 years", "last 5 years")
                            )
                            
                            
                        ),
                        
                        box(
                            title = "Opening Prices Chart",
                            width = 9,
                            solidHeader = TRUE,
                            status = "primary",
                            plotOutput("openchart", height = 435),
                            height = 500
                        )
                    )
                    
                ),
                
                #High Chart
                tabItem(
                    tabName = "high_charts_subitem",
                    
                    fluidRow(
                        HTML("<center> <h1> <b> High Charts </b> </h1> </center> <br>")
                    ),
                    
                    fluidRow(
                        box(
                            title = "Set Parameters",
                            width = 3,
                            solidHeader = TRUE,
                            status = "primary",
                            height = 500,
                            
                            textInput("HighStockCode", "StockCode:", value = "AAPL"),
                            
                            dateRangeInput(
                                "HighDates",
                                label = tags$div(tags$b("Date Range:")),
                                format = "dd-mm-yy",
                                start = Sys.Date() - 2000,
                                end = Sys.Date()
                            ),
                            
                            radioButtons(
                                inputId = "HighPeriod",
                                label = tags$div(tags$b("Periodicity:")),
                                choices = c("daily", "weekly", "monthly")
                            ),
                            
                            radioButtons(
                                inputId = "HighTimePeriod",
                                label = tags$div(tags$b("Time:")),
                                choices = c("last 1 year", "last 3 years", "last 5 years")
                            )
                            
                            
                        ),
                        
                        box(
                            title = "High Prices Chart",
                            width = 9,
                            solidHeader = TRUE,
                            status = "primary",
                            plotOutput("highchart", height = 435),
                            height = 500
                        )
                    )
                ),
                
                #Low Chart 
                tabItem(
                    tabName = "low_charts_subitem",
                    
                    fluidRow(
                        HTML("<center> <h1> <b> Low Charts </b> </h1> </center> <br>")
                    ),
                    
                    fluidRow(
                        box(
                            title = "Set Parameters",
                            width = 3,
                            solidHeader = TRUE,
                            status = "primary",
                            height = 500,
                            
                            textInput("LowStockCode", "StockCode:", value = "AAPL"),
                            
                            dateRangeInput(
                                "LowDates",
                                label = tags$div(tags$b("Date Range:")),
                                format = "dd-mm-yy",
                                start = Sys.Date() - 2000,
                                end = Sys.Date()
                            ),
                            
                            radioButtons(
                                inputId = "LowPeriod",
                                label = tags$div(tags$b("Periodicity:")),
                                choices = c("daily", "weekly", "monthly")
                            ),
                            
                            radioButtons(
                                inputId = "LowTimePeriod",
                                label = tags$div(tags$b("Time:")),
                                choices = c("last 1 year", "last 3 years", "last 5 years")
                            )
                            
                        ),
                        
                        box(
                            title = "Low Prices Chart",
                            width = 9,
                            solidHeader = TRUE,
                            status = "primary",
                            plotOutput("lowchart", height = 435),
                            height = 500
                        )
                    )
                ),
                
                #Close Chart  
                tabItem(
                    tabName = "close_charts_subitem",
                    
                    fluidRow(
                        HTML("<center> <h1> <b> Close Charts </b> </h1> </center> <br>")
                    ),
                    
                    fluidRow(
                        box(
                            title = "Set Parameters",
                            width = 3,
                            solidHeader = TRUE,
                            status = "primary",
                            height = 500,
                            
                            textInput("CloseStockCode", "StockCode:", value = "AAPL"),
                            
                            dateRangeInput(
                                "CloseDates",
                                label = tags$div(tags$b("Date Range:")),
                                format = "dd-mm-yy",
                                start = Sys.Date() - 2000,
                                end = Sys.Date()
                            ),
                            
                            radioButtons(
                                inputId = "ClosePeriod",
                                label = tags$div(tags$b("Periodicity:")),
                                choices = c("daily", "weekly", "monthly")
                            ),
                            
                            radioButtons(
                                inputId = "CloseTimePeriod",
                                label = tags$div(tags$b("Time:")),
                                choices = c("last 1 year", "last 3 years", "last 5 years")
                            )
                        ),
                        
                        box(
                            title = "Close Prices Chart",
                            width = 9,
                            solidHeader = TRUE,
                            status = "primary",
                            plotOutput("closechart", height = 435),
                            height = 500
                        )   
                    )
                ),
                
                #advisor_table
                tabItem(
                    "Advisor_Table",
                    tags$style(type = 'text/css', 'html, body {width:100%;height:100%}'),
                    tags$title("Stock Market analysis by CAPM"),
                    
                    fluidRow(
                        HTML("<center> <h1> <b> Stock Suggestor based on the CAPM Model</b> </h1> </center> <br>")
                    ),
                    
                    
                    fluidRow(column(
                        4,
                        tags$h4("Parameters"),
                        wellPanel(
                            fluidRow(column(
                                4,
                                textInput("indexTicker", "Index", value = "BSE", width = "100%")
                            ),
                            column(
                                8,
                                selectInput("tickersList", "Tickers list", c("tickers.csv"), width = "100%")
                            )),
                            fluidRow(
                                column(
                                    4,
                                    dateInput(
                                        inputId = "dateFrom",
                                        label = "From",
                                        value = seq(Sys.Date(), length = 2, by = "-364 days")[2],
                                        format = "yyyy-mm-dd",
                                        width = "90%"
                                    )
                                ),
                                column(
                                    4,
                                    dateInput(
                                        inputId = "dateTo",
                                        label = "To",
                                        value = Sys.Date(),
                                        format = "yyyy-mm-dd",
                                        width = "90%"
                                    )
                                ),
                                column(
                                    4,
                                    numericInput(
                                        inputId = "rfrInput",
                                        label = "Riskfree rate %",
                                        value = 6.5,
                                        min = 0.0,
                                        max = 100.0,
                                        step = 0.25,
                                        width = "110%"
                                    )
                                )
                            ),
                            tags$b("Pick a last period quickly"),
                            fluidRow(
                                column(
                                    3,
                                    actionButton(
                                        inputId = "last1Button",
                                        label = "1 month",
                                        width = "120%"
                                    )
                                ),
                                column(
                                    3,
                                    actionButton(
                                        inputId = "last3Button",
                                        label = "3 months",
                                        width = "120%"
                                    )
                                ),
                                column(
                                    3,
                                    actionButton(
                                        inputId = "last6Button",
                                        label = "6 months",
                                        width = "120%"
                                    )
                                ),
                                column(
                                    3,
                                    actionButton(
                                        inputId = "lastYearButton",
                                        label = "1 year",
                                        width = "120%"
                                    )
                                )
                            )
                        )
                    ),
                    column(
                        8,
                        tags$h4("Analyzed stocks"),
                        fluidRow(column(12,
                                        wellPanel(
                                            DT::dataTableOutput(outputId = "stocksTable", width = "100%")
                                        )))
                    )),
                    
                    fluidRow(
                        wellPanel(
                            tags$h4("BSE Stock Prices"),
                            wellPanel(
                                DT::dataTableOutput(outputId = "BSEstocksTable", width = "100%")
                            )  
                        )
                    ),
                    fluidRow(
                        wellPanel(
                            tags$h4("NSE Stock Prices"),
                            wellPanel(
                                DT::dataTableOutput(outputId = "NSEstocksTable", width = "100%")
                            )
                        )
                    )
                ),
                
                #built by  
                tabItem(
                    tabName = "builtby",
                    
                    h1("Guide"),
                    h2("Dr. A. Sharada"),
                    
                    h1("Team members"),
                    h2("Alekhya Tethali"),
                    h2("Patlola Preethi"),
                    h2("T. Vineela"),
                    h2("Merugu Shanthi")
                )
                            ) 
            
            
    } else {
        loginpart
    }
})
    
    #Auto.Arima - plot here
    output$auto.arima <- renderPlot({
        # if (is.null(input$StockCode))
        #   return()
        
        #Stock <- as.character(input$StockCode)
        
        data <- eventReactive(input$click, {
            (input$StockCode)
        })
        Stock <- as.character(data())
        print(Stock)
        Stock_df <- as.data.frame(
            getSymbols(
                Symbols = Stock,
                src = "yahoo",
                from = "2017-01-01",
                env = NULL
            )
        )
        
        Stock_df$Open = Stock_df[, 1]
        Stock_df$High = Stock_df[, 2]
        Stock_df$Low = Stock_df[, 3]
        Stock_df$Close = Stock_df[, 4]
        Stock_df$Volume = Stock_df[, 5]
        Stock_df$Adj = Stock_df[, 6]
        Stock_df <- Stock_df[, c(7, 8, 9, 10, 11, 12)]
        
        #plot(as.ts(Stock_df$Close))
        
        Stock_df$v7_MA = ma(Stock_df$Close, order = 7)
        Stock_df$v30_MA <- ma(Stock_df$Close, order = 30)
        
        #STL
        rental_ma <- ts(na.omit(Stock_df$v7_MA), frequency = 30)
        decomp_rental <- stl(rental_ma, s.window = "periodic")
        #plot(decomp_rental)
        adj_rental <- seasadj(decomp_rental)
        #plot(adj_rental)
        
        
        #arima
        fit <- auto.arima(Stock_df$Close, ic = "bic")
        fit.forecast <- forecast(fit)
        plot(fit.forecast,  main = Stock)
        fit.forecast
        
    })
    
    #Auto.Arima1 - plot here  Table output
    output$auto.arima1 <- renderTable({
        #if (is.null(input$StockCode))
        #  return()
        
        
        #Stock <- as.character(input$StockCode)
        data <- eventReactive(
            input$click, {(
                input$StockCode
            )}
        )
        
        Stock <- as.character(data())
        
        print(Stock)
        Stock_df <- as.data.frame(
            getSymbols(
                Symbols = Stock,
                src = "yahoo",
                from = "2017-01-01",
                env = NULL
            )
        )
        
        Stock_df$Open = Stock_df[, 1]
        Stock_df$High = Stock_df[, 2]
        Stock_df$Low = Stock_df[, 3]
        Stock_df$Close = Stock_df[, 4]
        Stock_df$Volume = Stock_df[, 5]
        Stock_df$Adj = Stock_df[, 6]
        Stock_df <- Stock_df[, c(7, 8, 9, 10, 11, 12)]
        
        #plot(as.ts(Stock_df$Close))
        
        Stock_df$v7_MA = ma(Stock_df$Close, order = 7)
        Stock_df$v30_MA <- ma(Stock_df$Close, order = 30)
        
        #STL
        rental_ma <- ts(na.omit(Stock_df$v7_MA), frequency = 30)
        decomp_rental <- stl(rental_ma, s.window = "periodic")
        #plot(decomp_rental)
        adj_rental <- seasadj(decomp_rental)
        #plot(adj_rental)
        
        
        #arima
        #auto.arima() Returns best ARIMA model according to either AIC, AICc or BIC value.
        fit <- auto.arima(Stock_df$Close, ic = "bic")
        fit.forecast <- forecast(fit)
        #plot(fit.forecast,   col = "red")
        print("fit.forecast")
        
        table_to_add <- (fit.forecast)
        #d1 <- Sys.Date()
        #d2 <- d1 + 10
        #table_for_dates <- seq.Date(d1,d2, by = "days")
        #final_table_added_dates <- rbindlist(list(as.data.frame(table_for_dates),table_to_add),fill = TRUE)
        #print(final_table_added_dates)
        #final_table_added_dates
        
        table_to_add
    })
    #Stock Price on a particular date
    output$stock.ondate <- renderUI({
        print("entered output$stock.ondate")
        data_date <- eventReactive(
            input$check, {(
                input$Dates
            )}
        )
        ddate <- data_date
        data <- eventReactive(
            input$click, {(
                input$StockCode
            )}
        )
        Stock <- as.character(data())
        print(Stock)
        
        dayf = data.frame(date = Sys.Date() - 1)
        dayf$day <- weekdays(as.Date(dayf$date))
        dayf <- as.character(dayf)
        
        print("Conversion to weekdays finished")
        
        #confirming button click
        if (input$Dates == 0)
        {
            return()
        }
        
        if (dayf[2] == "Sunday" || is.null(dayf[2]))
            print("No stock exchanges on this day")
        else
        {
            input$Dates
            Stock_date <- getSymbols(
                Symbols = Stock,
                src = "yahoo",
                env = NULL,
                from = as.Date(input$Dates),
                to =  as.Date(input$Dates) + 1
            )
            
            Stock_on <- as.data.frame(Stock_date)
            print(Stock_on)
            
            String_Open <- paste("Open Price:", Stock_on[,1])
            String_High <- paste("High Price:", Stock_on[,2])
            String_Low <- paste("Low Price:", Stock_on[,3])
            String_Close <- paste("Close Price:", Stock_on[,4])
            
            HTML(paste(String_Open, String_High, String_Low, String_Close, sep = '<br/>'))
            
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
    })
    
    #Dividend table
    output$dividend_display_table <- renderTable({
        stock_dividend <- as.data.frame(
            getDividends(
                Symbol = input$Dividend_StockCode,
                src = "yahoo",
                from = "2017-01-01",
                env = NULL
            )
        ) 
        
        setDT(stock_dividend, keep.rownames = TRUE)[]
        names(stock_dividend)[names(stock_dividend)=="rn"] <- "Dates"
        
        #stock_dividend1 <- stock_dividend[order(as.Date(stock_dividend$Dates)),]
        #stock_dividend2 <- rev(order(as.Date(stock_dividend$Dates)))
        #print(stock_dividend1)
        #print(stock_dividend2$Dates)
        
        stock_dividend1 <- stock_dividend[nrow(stock_dividend):1,]
        stock_dividend2 <- split.data.frame(stock_dividend1,3)
        print(stock_dividend2)
        
        return(stock_dividend1)
    })
    
    #Open Chart
    output$openchart <- renderPlot({
        
        OpenData <- getSymbols(
            input$OpenStockCode,
            src = "yahoo",
            from = input$OpenDates[1],
            to = input$OpenDates[2],
            auto.assign = FALSE,
            periodicity = input$OpenPeriod
        )
        
        print(
            chartSeries(
                OpenData[,1],
                theme = chartTheme("white"),
                up.col = "green",
                dn.col = "red",
                TA = NULL,
                name = input$OpenStockCode,
                subset = input$OpenTimePeriod
            )
        )
    })
    
    #High Chart
    output$highchart <- renderPlot({
        
        HighData <- getSymbols(
            input$HighStockCode,
            src = "yahoo",
            from = input$HighDates[1],
            to = input$HighDates[2],
            auto.assign = FALSE,
            periodicity = input$HighPeriod
        )
        
        print(
            chartSeries(
                HighData[,2],
                theme = chartTheme("white"),
                up.col = "green",
                dn.col = "red",
                TA = NULL,
                name = input$HighStockCode,
                subset = input$HighTimePeriod
            )
        )
    })
    
    #Low Chart
    output$lowchart <- renderPlot({
        
        LowData <- getSymbols(
            input$LowStockCode,
            src = "yahoo",
            from = input$LowDates[1],
            to = input$LowDates[2],
            auto.assign = FALSE,
            periodicity = input$LowPeriod
        )
        
        print(
            chartSeries(
                LowData[,3],
                theme = chartTheme("white"),
                up.col = "green",
                dn.col = "red",
                TA = NULL,
                name = input$LowStockCode,
                subset = input$LowTimePeriod
            )
        )
    })
    
    #Close Chart
    output$closechart <- renderPlot({
        
        CloseData <- getSymbols(
            input$CloseStockCode,
            src = "yahoo",
            from = input$CloseDates[1],
            to = input$CloseDates[2],
            auto.assign = FALSE,
            periodicity = input$ClosePeriod
        )
        
        print(
            chartSeries(
                CloseData[,4],
                theme = chartTheme("white"),
                up.col = "green",
                dn.col = "red",
                TA = NULL,
                name = input$CloseStockCode,
                subset = input$CloseTimePeriod
            )
        )
    })
    
    #advisor_table part
    generalData <- reactive({
        stocks <-
            read.csv2(paste0('./data/', input$tickersList),
                      stringsAsFactors = F)
        days.ago <-
            difftime(input$dateTo, input$dateFrom, units = c("days"))
        rf.rate = as.numeric(input$rfrInput / 365 * days.ago)
        #risk free return percentage is the percentage of appreciation of the investment with 0(zero) risk
        
        p <- getSymbols(
            input$indexTicker,
            from = input$dateFrom,
            to = input$dateTo,
            src = "yahoo",
            auto.assign = FALSE
        )
        
        m <- p[, -6]
        
        market.prices <-    as.numeric( m[, 4] )
        print("market.prices")
        print(market.prices)
        market.returns <-
            na.omit(diff(market.prices) / market.prices[1:(length(market.prices) - 1)])
        print("market.returns")
        print(market.returns)
        m.return <-
            (market.prices[length(market.prices)] - market.prices[1]) / market.prices[1] * 100
        print("m.return")
        print(m.return)
        #in line 20 t(stock) - t() func is for matrix transpose => its output is:
        #            [,1]   [,2]    [,3]
        #   ticker "SBER" "SBERP" "GAZP"
        withProgress(message = "Loading stocks data", value = 0, {
            asset.prices <- sapply(t(stocks),
                                   function(x) {
                                       print("asset.prices - function(x)")
                                       print(x)
                                       incProgress(1 / nrow(stocks), detail = x)
                                       
                                       a <- getSymbols(
                                           as.character(x),
                                           from = input$dateFrom,
                                           to = input$dateTo,
                                           src = "yahoo",
                                           auto.assign = F
                                       )
                                       
                                       b <- a[,-6]
                                       as.numeric(
                                           b[, 4]
                                       )
                                   },
                                   simplify = FALSE, USE.NAMES = TRUE)
            print("asset.prices")
            print(asset.prices)
            stocks.df <-
                data.frame(
                    ticker = names(asset.prices),
                    beta = rep(NA, nrow(stocks)),
                    expected.return = rep(NA, nrow(stocks)),
                    return = rep(NA, nrow(stocks)),
                    alpha = rep(NA, nrow(stocks)),
                    r2 = rep(NA, nrow(stocks)),
                    sortino = rep(NA, nrow(stocks))
                )
            print("stock.df")
            print(stocks.df)
        })
        
        withProgress(message = "Analyzing price data", value = 0, {
            print("while analyzing price data")
            stocks.df[, c("beta",
                          "expected.return",
                          "return",
                          "alpha",
                          "r2",
                          "sortino")] <-
                t(as.data.frame(lapply(asset.prices,
                                       function(x) {
                                           print("inside lapply - function(x)")
                                           print(x)
                                           incProgress(1 / nrow(stocks))
                                           
                                           print("asset.returns")
                                           asset.returns <- na.omit(diff(x) / x[1:(length(x) - 1)])
                                           print(asset.returns)
                                           
                                           print("beta")
                                           if(length(asset.returns) > length(market.returns))
                                               asset.returns <- na.omit(asset.returns[1:length(market.returns)])
                                           else if(length(market.returns) > length(asset.returns))
                                               market.returns <- na.omit(market.returns[1:length(asset.returns)])
                                           beta = cov(asset.returns, market.returns) / var(market.returns)
                                           print(beta)
                                           
                                           print("lm.df")
                                           if(length(market.prices) > length(x))
                                               market.prices <- na.omit(market.prices[1:length(x)])
                                           else if(length(x) > length(market.prices))
                                               x <- na.omit(x[1:length(market.prices)])
                                           lm.df = data.frame(market = market.prices, asset = x)
                                           print(lm.df)
                                           
                                           print("lm.fit")
                                           lm.fit = lm(formula = market ~ asset, data = lm.df)
                                           print(lm.fit)
                                           
                                           print("r2")
                                           r2 = summary(lm.fit)$adj.r.squared
                                           print(r2)
                                           
                                           print("expected.return")
                                           expected.return = rf.rate + beta * (m.return - rf.rate)
                                           print(expected.return)
                                           
                                           print("stock.return")
                                           stock.return = (x[length(x)] - x[1]) / x[1] * 100
                                           print(stock.return)
                                           
                                           print("alpha")
                                           alpha = stock.return - expected.return
                                           print(alpha)
                                           
                                           print("sortino")
                                           sortino = SortinoRatio(R = asset.returns)[, 1]
                                           print(sortino)
                                           
                                           print("rounding")
                                           round(c(
                                               beta,
                                               expected.return,
                                               stock.return,
                                               alpha,
                                               r2,
                                               sortino
                                           ),
                                           4)
                                           print(round(c(
                                               beta,
                                               expected.return,
                                               stock.return,
                                               alpha,
                                               r2,
                                               sortino
                                           ),
                                           4))
                                       })))
        })
        
        step = 0.1
        
        print("sml")
        sml <-
            rf.rate + seq(0, 2.5, by = step) * (m.return - rf.rate)
        print(sml)
        
        print("slope")
        slope = (sml[2] - sml[1]) / step
        print(slope)
        
        print("final return")
        print(list(stocks.df,
                   rf.rate,
                   slope,
                   market.prices,
                   asset.prices,
                   stocks))
        list(stocks.df,
             rf.rate,
             slope,
             market.prices,
             asset.prices,
             stocks)
    })
    
    output$stocksTable <- DT::renderDataTable({
        stocks.df <- generalData()[[1]]
        stocks.df#[, c("ticker", "beta", "alpha", "r2")]
    }, server = TRUE, selection = "single",
    options = list(
        rowCallback = JS(
            'function(row, data) {
            if (data[5] > 0)
            $("td", row).css("color", "green");
            else if (data[5] < 0)
            $("td", row).css("color", "red");
}'
  ),
  paging = FALSE,
  searching = FALSE,
  processing = FALSE
        ))
    
    output$BSEstocksTable <- DT::renderDataTable({
        bsestocks <- as.data.frame(getSymbols(
            Symbols = "BSE",
            src = "yahoo",
            env = NULL,
            from = input$dateFrom,
            to = input$dateTo
        )
        )
        
        setDT(bsestocks, keep.rownames = TRUE)[]
        names(bsestocks)[names(bsestocks)=="rn"] <- "Date"
        
        bsestocks
    })
    
    output$NSEstocksTable <- DT::renderDataTable({
        nsestocks <- as.data.frame(getSymbols(
            Symbols = "BSE",
            src = "yahoo",
            env = NULL,
            from = input$dateFrom,
            to = input$dateTo
        )
        )
        
        setDT(nsestocks, keep.rownames = TRUE)[]
        names(nsestocks)[names(nsestocks)=="rn"] <- "Date"
        
        nsestocks
    })
    
    # Whenever a field is filled, aggregate all form data
    formData <- reactive({
        data <- sapply(fields, function(x)
            input[[x]])
        data
    })
    
    # When the Submit button is clicked, save the form data
    observeEvent(input$Signup, {
        saveData(formData())
    })
    
    saveData <- function(data) {
        # Connect to the database
        drv <- dbDriver("PostgreSQL")
        
        con <- dbConnect(
            drv,
            dbname = "miniproject",
            host = "localhost",
            port = 5432,
            user = "postgres",
            password = "Alekhya"
        )
        
        # Construct the update query by looping over the data fields
        #query <- paste0("INSERT INTO public.login (username) VALUES ( $1 )")
        # Submit the update query and disconnect
        #dbSendQuery(con, query, params=data[["username"]])
        query <- sprintf(
            "INSERT INTO public.login (%s) VALUES ('%s')",
            paste(names(data), collapse = ", "),
            paste(data, collapse = "', '")
        )
        # Submit the update query and disconnect
        dbGetQuery(con, query)
        dbDisconnect(con)
    }
    
    # (update with current response when Submit is clicked)
    output$responses <- DT::renderDataTable({
        input$Signup
    })
    
    observeEvent(input$Signup, {
        showModal(modalDialog(
            title = "You have signed up successfully!",
            paste0("Now you can Login to 'Investment Advisor'. "),
            easyClose = TRUE,
            footer = NULL
        ))
    })
    
    observeEvent(input$last1Button, {
        value.from <- seq(Sys.Date(), length = 2, by = "-29 days")[2]
        value.to <- Sys.Date()
        updateDateInput(session, "dateFrom", value = value.from)
        updateDateInput(session, "dateTo", value = value.to)
    })
    
    observeEvent(input$last3Button, {
        value.from <- seq(Sys.Date(), length = 2, by = "-89 days")[2]
        value.to <- Sys.Date()
        updateDateInput(session, "dateFrom", value = value.from)
        updateDateInput(session, "dateTo", value = value.to)
    })
    
    observeEvent(input$last6Button, {
        value.from <- seq(Sys.Date(), length = 2, by = "-181 days")[2]
        value.to <- Sys.Date()
        updateDateInput(session, "dateFrom", value = value.from)
        updateDateInput(session, "dateTo", value = value.to)
    })
    
    observeEvent(input$lastYearButton, {
        value.from <- seq(Sys.Date(), length = 2, by = "-364 days")[2]
        value.to <- Sys.Date()
        updateDateInput(session, "dateFrom", value = value.from)
        updateDateInput(session, "dateTo", value = value.to)
    })
    }
shinyApp(ui = ui, server = server)