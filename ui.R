
library(shinydashboard)

ui <- dashboardPage(skin = "green",
                    dashboardHeader(title = "Forecasting solution"),
                    
                    dashboardSidebar(width = 300,
                      tags$head(
                        tags$style(HTML("
                                        .sidebar { height: 90vh; overflow-y: auto; }
                      
                                                          " ))),
                    
                        
                     
                      sidebarMenu(
                        #menuItem("Introduction", tabName = "intro", icon = icon("th")),
                        # menuItem("Data", tabName = "data", icon = icon("dashboard")),
                        menuItem("Data Files", tabName = "Files", icon = icon("dashboard")),
                        menuItem("Forecast", tabName = "forecast", icon = icon("dashboard")),
                        
                        
                        tags$div(
                        htmlOutput("select_product"),
                        htmlOutput("select_store"),
                        
                        fluidRow(
                          
                        tags$div(column(5,radioButtons("flevel","Forecast:",inline = F,
                                              list("Weekly"="Weekly","Monthly"="Monthly")),style="")),
                        tags$div(column(5,radioButtons("flevel2","Location:",inline = F,
                                              list("Store"="Store","Channel"="Channel")),style=""))),
                        
                        selectInput("var", "Forecasts:",
                                    list("Arima" = "Arima", 
                                         "Linear Model" = "Linear Model",
                                         "STLF" = "STLF",
                                         "Basic Exponential Smoothing" = "Basic Exponential Smoothing",
                                         "Double Exponential Smoothing" = "Double Exponential Smoothing",
                                         "Triple Exponential Smoothing" = "Triple Exponential Smoothing")),
                        numericInput("ahead", "Weeks to Forecast Ahead:", 6),
                        
                        fluidRow(
                          tags$div(column(6,offset=0,submitButton("Generate",width = '100px'),style="position:relative;left:15px;")), 
                          column(6,downloadButton('dData', 'Download',class="butt"),
                                 tags$head(tags$style(".butt{background-color:#008B8B;}")))
                        )))
                      
                      
                    ),
                    dashboardBody(
                      
                      tabItems(
                                                # FirST tab content
                        tabItem(tabName = "Files",
                                tags$div(wellPanel(fluidRow(
                                  tags$div(fileInput('file1', 'Load Sales File',
                                            accept = c('text/csv','text/comma-separated-values',
                                                       'text/tab-separated-values','text/plain','.csv','.tsv'))),
                                  
                                  tags$div(fileInput('file2', 'Load Calendar Hierarchy',
                                                     accept = c('text/csv','text/comma-separated-values',
                                                                'text/tab-separated-values','text/plain','.csv','.tsv'))),
                                  tags$div(fileInput('file3', 'Load Product Hierarchy',
                                                     accept = c('text/csv','text/comma-separated-values',
                                                                'text/tab-separated-values','text/plain','.csv','.tsv'))),
                                  tags$div(fileInput('file4', 'Load Location Hierarchy',
                                                     accept = c('text/csv','text/comma-separated-values',
                                                                'text/tab-separated-values','text/plain','.csv','.tsv')))
                                         ))),style="background-color:#FAEBD7;"),
                        
                        # h4("This is a forecasting tool created by shiny.  
                        #  It takes inputs from csv and segrates the data. The data is fed to forecasting solution
                        #  to predict future values or forecasts. Several forecasting algorithms are taken into consideration.
                        #  The best forecast is provided to you")),
                        # Second tab content
                        
                        # Fourt tab content
                        tabItem(tabName = "forecast",
                                fluidRow(
                                  tabBox(width=13,
                                         # Title can include an icon
                                         title = tagList(shiny::icon("gear"), "Forecasts"),
                                         tabPanel("Sales",
                                                  wellPanel(dataTableOutput("table"))),
                                         tabPanel("Selected Forecast", 
                                                  wellPanel(fluidRow(
                                                   tags$em(tags$u( h4("Forecast Accuracy "))),   
                                                    verbatimTextOutput("facc"),
                                                    hr(),
                                                   # tags$em(tags$u(h4("Forecasted Sales"))),
                                                   #  dataTableOutput("fcast"),
                                                    # hr(),
                                                   plotlyOutput("sgraph")))),
                                         tabPanel("Forecast Comparison", plotlyOutput("fplot",height = "540px")),
                                         tabPanel("Best Forecast", 
                                                  wellPanel(fluidRow(
                                                    valueBoxOutput("mape",5),
                                                    valueBoxOutput("mod",7),
                                                    dataTableOutput("bfcast"),
                                                    plotlyOutput("graph")
                                                    )))
                                         
                                  )
                                )
                        )
                        
                        
                      )
                    )
                    )
