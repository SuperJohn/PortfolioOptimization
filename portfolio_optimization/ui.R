#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(lubridate)
library(shinycssloaders)
library(shinythemes)

stocks <- c("SPY", "IYR", "QQQ", "TLT", "GLD", "IWM", "EEM", "DBC", "EFA")

# Define UI for application that draws a histogram
shinyUI(
  navbarPage("GL Capital | Money Maker 1.0", selected = "Momentum",
     tags$head(tags$style(HTML("hr {border-top: 1px solid #000000;}"))),
     theme = shinytheme("paper"), # shinythemes::themeSelector(),
     # titlePanel("Gordon Long Capital"),
     # h4("Money Maker beta"),
     tabPanel("Momentum",
        # Sidebar with a slider input for number of bins 
        sidebarLayout(
          sidebarPanel(width=2,
             actionButton("go", "Pull Data", width = "100%"),
             h5("Select Stocks"),
             selectInput('in6', NULL, stocks, selected = stocks, multiple=TRUE, selectize=TRUE),
             h5("Momentum Lookback"),
             numericInput("momLookback", "(months)", 6)#,
             # hr(), 
             # h5("Download Data"), 
             # Button
             # downloadButton("downloadData", "Download")
          ),
          # Show a plot of the generated distribution
          mainPanel(
            tabsetPanel(
              tabPanel("Stock Prices",
                       withSpinner(plotOutput("stockPlot", height="700px"), type = 8)),
              tabPanel("Returns",
                       withSpinner(plotOutput("returnsPlot", height="700px"), type = 8)),
              tabPanel("Winners: Top 5",
                       withSpinner(plotOutput("growthPlot"), type = 8), 
                       hr(), 
                       withSpinner(plotOutput("winnersPlot"), type = 8)
                       )
            ) #end tabsetPanel
          )#end mainPanel
        )#end sidebarLayout
        ),
     tabPanel("Optimization",
        sidebarLayout(
          sidebarPanel(width=2, 
             actionButton("optimize", "Run Optimization", width = "100%"),
             h5("Optimized Min Variance:"),
             withSpinner(textOutput("opt_min_var", h5))
            ),#end sidebarPanel
          mainPanel(
            tabsetPanel(
              tabPanel("Optimization Results",
                       h5("Optimal Weights"),
                       withSpinner(tableOutput("opt_weights"), type = 8),
                       withSpinner(plotOutput("weights_pie"), type = 8),
                       hr(),
                       h5("Risk / Return Possibility Frontier"),
                       withSpinner(plotOutput("rr"), type = 8)
                       ),
              tabPanel("Portfolio Growth", withSpinner(plotOutput("port_growth"), type = 8)),
              tabPanel("Efficiency Frontier", withSpinner(plotOutput("ef"), type = 8))
              )#end tabsetPanel
            )#end mainPanel
          )#end sidebarLayout
     ),#end tabPanel
     
     tabPanel("BackTest",
              sidebarLayout(
                sidebarPanel(width=2, 
                   actionButton("backtest", "Run Backtest", width = "100%"),
                   h5("Training Period"),
                   # p("an integer of the number of periods to use as a training data in the front of the returns data"),
                   numericInput("training_period", "months (max=36)", 36),
                   h5("Rolling Window"),
                   # p("an integer of the width (i.e. number of periods) of the rolling window, the default of NULL will run the optimization using the data from inception."),
                   numericInput("rolling_window", "(months)", 12),
                   br(), 
                   h5("Annualized Returns"),
                   withSpinner(textOutput("bt_yr_ret", h5)), 
                   h5("Annualized Variance (StdDev)"),
                   withSpinner(textOutput("bt_yr_StdDev", h5))
                ),#end sidebarPanel
                mainPanel(
                  tabsetPanel(
                    tabPanel("Performance Summary",
                      withSpinner(plotOutput("bt_perf", height = "600px"), type = 8)
                    ),
                    tabPanel("Weights",
                       withSpinner(plotOutput("bt_weights"), type = 8)
                    )
                  )#end tabsetPanel
                )#end mainPanel
              )#end sidebarLayout
              )#end tabPanel
                
  
  )#end fluidPage
)#end shinyUI
